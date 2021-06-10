#include "Main.h"

#include <fstream>

#include <wx/gbsizer.h>
#include <wx/listbox.h>
#include <wx/stattext.h>
#include <wx/radiobox.h>
#include <wx/menu.h>
#include <wx/msgdlg.h>

#include "Engine.h"
#include "generic/std_glm.h"
#include "generic/LoadFile.h"
#include "render/rendertarget/canvas/EngineCanvas.h"
#include "scene/Scene.h"
#include "scene/model/Model.h"
#include "scene/Camera.h"
#include "render/RenderMode.h"
#include "network/EngineConnection.h"

#include "Chat.h"
#include "ServerPicker.h"

constexpr char SETTINGS_FILE[] = "settings.json";
constexpr char DEFAULT_SETTINGS_FILE[] = "settings.default.json";

Main::Main() : wxFrame(nullptr, wxID_ANY, "Render Test")
{
	//set up menu bar
	this->m_mb = new wxMenuBar();

	wxMenu* menu_server = new wxMenu();
	menu_server->Bind(wxEVT_MENU, &Main::mb_Server_Chat, this, menu_server->Append(wxID_ANY, "Chat", "Open a new chat window")->GetId());
	menu_server->Bind(wxEVT_MENU, &Main::mb_Server_Picker, this, menu_server->Append(wxID_ANY, "Server picker", "Open a new chat window")->GetId());
	this->m_mb->Append(menu_server, "Server");

	this->SetMenuBar(this->m_mb);

	//load settings file
	this->ReloadSettings();

	//register server packet handler
	this->Bind(EVT_CONNECTION, &Main::HandleConnectionEvent, this);

	//configure window
	this->SetBackgroundColour(wxColour(238, 238, 238));

	this->SetSize(this->FromDIP(wxSize(800, 600)));
	this->SetMinSize(this->FromDIP(wxSize(500, 400)));

	//make sizer
	this->m_sizer = new wxGridBagSizer(0, 0);
	this->m_sizer->SetFlexibleDirection(wxBOTH);
	this->m_sizer->SetNonFlexibleGrowMode(wxFLEX_GROWMODE_SPECIFIED);

	//create glcanvas
	this->m_scene = std::unique_ptr<Scene>(SceneFromJSON(this->GetSceneLoaderConfig()));

	this->m_engine = std::make_unique<Engine>(this, this->m_scene.get(), true);
	this->m_engine->SetDebugMessageLevel(std::vector({
		Engine::DebugMessageConfig({ GL_DONT_CARE, GL_DONT_CARE, GL_DEBUG_SEVERITY_NOTIFICATION, false })
		}));

	this->m_camera = std::make_unique<Camera>();
	this->m_camera->SetFOV(90.0);
	this->m_camera->SetRotation(90.0, 0.0, 0.0);
	this->m_camera->SetClips({ 0.1, 50.0 });
	this->m_camera->SetSSRRegionDimensions(glm::ivec2(64));

	RenderMode render_mode = RenderMode::Normal;
	int render_mode_index = 0;
	if (this->GetSettings()["render mode"].is_string())
	{
		if (this->GetSettings()["render mode"].get<std::string>() == "normal")
		{
			render_mode = RenderMode::Normal;
			render_mode_index = 0;
		}
		else if (this->GetSettings()["render mode"].get<std::string>() == "wireframe")
		{
			render_mode = RenderMode::Wireframe;
			render_mode_index = 1;
		}
		else if (this->GetSettings()["render mode"].get<std::string>() == "textured")
		{
			render_mode = RenderMode::Textured;
			render_mode_index = 2;
		}
		else
		{
			throw std::runtime_error("Setting for render mode is unknown: " + this->GetSettings()["render mode"].get<std::string>());
		}
	}

	this->m_glcanvas_controller = this->m_engine->GenerateNewCanvas(render_mode, wxID_ANY, this);

	this->m_glcanvas = this->m_glcanvas_controller->GetEngineCanvas();
	this->m_glcanvas->SetControlledCamera(this->m_camera.get());
	this->m_glcanvas->SetMouselook(true);
	this->m_glcanvas->SetKeyboardMove(true);

	this->m_sizer->Add(this->m_glcanvas, wxGBPosition(0, 0), wxGBSpan(5, 1), wxEXPAND | wxALL);

	//make list to display all model identifiers
	this->m_lb_models = new wxListBox(this, wxID_ANY);
	this->m_lb_models->Bind(wxEVT_LISTBOX, &Main::lb_models_OnSelection, this);
	this->m_lb_models->Bind(wxEVT_CHAR, &Main::lb_models_OnChar, this);
	this->m_sizer->Add(this->m_lb_models, wxGBPosition(0, 1), wxGBSpan(1, 2), wxEXPAND | wxALL);

	for (const std::shared_ptr<Model> model : this->m_scene->GetModels())
	{
		this->m_lb_models->Append(model->GetIdentifier());
	}

	//make vector controls for modifying models
	VectorCtrlConfig vct_cfg;
	vct_cfg.num_fields = 3;

	vct_cfg.inc = 1.0;
	this->m_vct_position = new VectorCtrl(this, wxID_ANY, vct_cfg);
	this->m_vct_position->Bind(EVT_VCTRCTRL_CHANGED, &Main::vct_position_OnChange, this);
	this->m_sizer->Add(this->m_vct_position, wxGBPosition(1, 2), wxGBSpan(1, 1), wxEXPAND | wxALL);

	vct_cfg.inc = 15.0;
	this->m_vct_rotation = new VectorCtrl(this, wxID_ANY, vct_cfg);
	this->m_vct_rotation->Bind(EVT_VCTRCTRL_CHANGED, &Main::vct_rotation_OnChange, this);
	this->m_sizer->Add(this->m_vct_rotation, wxGBPosition(2, 2), wxGBSpan(1, 1), wxEXPAND | wxALL);

	vct_cfg.inc = 1.0;
	this->m_vct_scale = new VectorCtrl(this, wxID_ANY, vct_cfg);
	this->m_vct_scale->Bind(EVT_VCTRCTRL_CHANGED, &Main::vct_scale_OnChange, this);
	this->m_sizer->Add(this->m_vct_scale, wxGBPosition(3, 2), wxGBSpan(1, 1), wxEXPAND | wxALL);

	//make labels for vector controls
	this->m_stxt_position = new wxStaticText(this, wxID_ANY, "Position");
	this->m_sizer->Add(this->m_stxt_position, wxGBPosition(1, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_stxt_rotation = new wxStaticText(this, wxID_ANY, "Rotation");
	this->m_sizer->Add(this->m_stxt_rotation, wxGBPosition(2, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_stxt_scale = new wxStaticText(this, wxID_ANY, "Scale");
	this->m_sizer->Add(this->m_stxt_scale, wxGBPosition(3, 1), wxGBSpan(1, 1), wxEXPAND | wxALL);

	//make render mode radio box
	wxArrayString render_modes = wxArrayString();
	render_modes.Add("Normal");
	render_modes.Add("Wireframe");
	render_modes.Add("Textured");

	this->m_rdobx_render_mode = new wxRadioBox(this, wxID_ANY, "Render Mode", wxDefaultPosition, wxDefaultSize, render_modes);
	this->m_rdobx_render_mode->Select(render_mode_index);
	this->m_rdobx_render_mode->Bind(wxEVT_RADIOBOX, &Main::rdobx_render_mode_OnChanged, this);
	this->m_sizer->Add(this->m_rdobx_render_mode, wxGBPosition(4, 1), wxGBSpan(1, 2), wxEXPAND | wxALL);

	//set window title
	this->SetTitle("Render Test: viewing " + this->m_scene->GetIdentifier());

	this->SetModel(nullptr);

	//start the render loop
	this->Bind(wxEVT_IDLE, &Main::Mainloop, this);

	//final layout configuration
	this->m_sizer->AddGrowableRow(0);
	this->m_sizer->AddGrowableCol(0);

	this->SetSizer(this->m_sizer);
	this->Centre(wxBOTH);
	this->Layout();
}

void Main::SetModel(std::shared_ptr<Model> model)
{
	if (this->m_model_selected != nullptr)
	{
		this->m_model_selected->SetCurrentWireframeIndex(0);
	}
	this->m_model_selected = model;

	if (model == nullptr)
	{
		this->m_lb_models->SetSelection(wxNOT_FOUND);

		this->m_vct_position->SetValues({ 0.0, 0.0, 0.0 });
		this->m_vct_rotation->SetValues({ 0.0, 0.0, 0.0 });
		this->m_vct_scale->SetValues({ 0.0, 0.0, 0.0 });

		this->m_vct_position->Disable();
		this->m_vct_rotation->Disable();
		this->m_vct_scale->Disable();
	}
	else
	{
		model->SetCurrentWireframeIndex(1);
			
		std::vector<std::shared_ptr<Model>> models = this->m_scene->GetModels();
		for (size_t i = 0; i < models.size(); i++)
		{
			if (models.at(i) == model)
			{
				this->m_lb_models->SetSelection(static_cast<int>(i));
			}
		}

		this->m_vct_position->Enable();
		this->m_vct_rotation->Enable();
		this->m_vct_scale->Enable();

		this->m_vct_position->SetValues(GetSTDVector(this->m_model_selected->GetPosition()));
		this->m_vct_rotation->SetValues(GetSTDVector(this->m_model_selected->GetRotation()));
		this->m_vct_scale->SetValues(GetSTDVector(this->m_model_selected->GetScale()));
	}
}

void Main::ReloadSettings()
{
	std::string settings_file = "";

	try
	{
		settings_file = LoadFile(SETTINGS_FILE);
	}
	catch (std::invalid_argument&)
	{
		settings_file = "";
	}

	if (settings_file == "")
	{
		try
		{
			std::filesystem::copy_file(
				std::filesystem::path(DEFAULT_SETTINGS_FILE),
				std::filesystem::path(SETTINGS_FILE),
				std::filesystem::copy_options::overwrite_existing
			);
		}
		catch (std::filesystem::filesystem_error&)
		{
			throw std::runtime_error("Couldn't copy default settings file into current settings file");
		}

		try
		{
			settings_file = LoadFile(SETTINGS_FILE);
		}
		catch (std::invalid_argument&)
		{
			settings_file = "";
		}
	}

	if (settings_file == "")
	{
		throw std::runtime_error("Couldn't open a settings file");
	}
	else
	{
		this->m_settings = nlohmann::json::parse(settings_file);
	}
}

nlohmann::json& Main::GetSettings()
{
	return this->m_settings;
}

const nlohmann::json& Main::GetSettings() const
{
	return this->m_settings;
}

const std::shared_ptr<EngineConnection>& Main::GetConnection() const
{
	return this->m_connection;
}

void Main::ConnectTo(std::string address, int port)
{
	this->m_connection = std::make_shared<EngineConnection>(address, port);
}

bool Main::IsConnected() const
{
	if (this->m_connection.get() == nullptr)
	{
		return false;
	}
	else
	{
		return this->m_connection->IsConnected();
	}
}

SceneLoaderConfig Main::GetSceneLoaderConfig() const
{
	SceneLoaderConfig config;

	config.path.root = this->GetSettings()["scene"]["root"].get<std::string>();
	config.path.file = this->GetSettings()["scene"]["file"].get<std::string>();

	config.performance.index = this->GetSettings()["performance level"].get<int>();

	return config;
}

void Main::mb_Server_Chat(wxCommandEvent& evt)
{
	if (this->IsConnected())
	{
		Chat* chat_window = new Chat(this);
		chat_window->Show();
	}
	else
	{
		wxMessageBox("You must connect to the server before you use the chat window", "Not connected");
	}

	evt.Skip();
}

void Main::mb_Server_Picker(wxCommandEvent& evt)
{
	ServerPicker* server_picker = new ServerPicker(this);
	server_picker->Show();

	evt.Skip();
}

void Main::lb_models_OnSelection(wxCommandEvent& evt)
{
	int selection_index = this->m_lb_models->GetSelection();
	if (selection_index != wxNOT_FOUND)
	{
		const std::vector<std::shared_ptr<Model>>& models = this->m_scene->GetModels();
		this->SetModel(models.at(selection_index));
	}
}

void Main::lb_models_OnChar(wxKeyEvent& evt)
{
	if (evt.GetKeyCode() == WXK_ESCAPE)
	{
		this->SetModel(nullptr);
	}
	evt.Skip();
}

void Main::rdobx_render_mode_OnChanged(wxCommandEvent& evt)
{
	int render_mode_index = evt.GetInt();

	RenderMode mode;
	if (render_mode_index == 0)
	{
		mode = RenderMode::Normal;
	}
	else if(render_mode_index == 1)
	{
		mode = RenderMode::Wireframe;
	}
	else if(render_mode_index == 2)
	{
		mode = RenderMode::Textured;
	}
	else
	{
		throw std::runtime_error("Unknown render mode selection (index: " + std::to_string(render_mode_index) + ", label: " + evt.GetString() + ")");
	}

	this->m_glcanvas_controller->SetRenderLayers(mode);

	evt.Skip();
}

void Main::vct_position_OnChange(VectorCtrlEvent& evt)
{
	if (this->m_model_selected != nullptr)
	{
		this->m_model_selected->SetPosition(GetGLMVector<3>(this->m_vct_position->GetValues()));
	}
	evt.Skip();
}

void Main::vct_rotation_OnChange(VectorCtrlEvent& evt)
{
	if (this->m_model_selected != nullptr)
	{
		this->m_model_selected->SetRotation(GetGLMVector<3>(this->m_vct_rotation->GetValues()));
	}
	evt.Skip();
}

void Main::vct_scale_OnChange(VectorCtrlEvent& evt)
{
	if (this->m_model_selected != nullptr)
	{
		this->m_model_selected->SetScale(GetGLMVector<3>(this->m_vct_scale->GetValues()));
	}
	evt.Skip();
}

void Main::Mainloop(wxIdleEvent& evt)
{
	this->m_engine->Render(true);

	if (this->IsConnected())
	{
		this->m_connection->ProcessOutstandingPackets(this);
	}

	evt.RequestMore();
	evt.Skip();
}

void Main::HandleConnectionEvent(ConnectionEvent& evt)
{
	if (evt.GetEvent().GetType() == EngineConnectionEvent::Type::PacketReceived)
	{
		const Packet& packet = evt.GetEvent().GetPacket();
		if (packet.GetType() == Packet::Type::ConnEstablished)
		{
			if (this->GetSettings().contains("network")
				&& this->GetSettings()["network"].is_object()
				&& this->GetSettings()["network"].contains("username")
				&& this->GetSettings()["network"]["username"].is_string()
				&& this->IsConnected())
			{
				this->m_connection->SendPacket(Packet(Packet::SetClientName(this->GetSettings()["network"]["username"].get<std::string>())));
			}
		}
	}
}
