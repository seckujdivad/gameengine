#pragma once

#include <memory>

#include <wx/frame.h>

#include <nlohmann/json.hpp>

#include "loaders/SceneLoader.h"
#include "network/wx/ConnectionEvent.h"

#include "VectorCtrl.h"

class wxGridBagSizer;
class wxListBox;
class wxStaticText;
class wxRadioBox;
class wxMenuBar;

class Engine;
class EngineCanvas;
class EngineCanvasController;
class Scene;
class Camera;
class Model;

class EngineConnection;

class Main : public wxFrame
{
private:
	//gameengine
	std::unique_ptr<Engine> m_engine;
	std::unique_ptr<Scene> m_scene;
	std::unique_ptr<Camera> m_camera;

	std::shared_ptr<Model> m_model_selected = nullptr;

	std::shared_ptr<EngineConnection> m_connection;

	nlohmann::json m_settings;

	SceneLoaderConfig GetSceneLoaderConfig() const;

	//wxwidgets
	wxGridBagSizer* m_sizer;

	wxMenuBar* m_mb;

	void mb_Server_Chat(wxCommandEvent& evt);
	void mb_Server_Picker(wxCommandEvent& evt);
	
	EngineCanvas* m_glcanvas;
	EngineCanvasController* m_glcanvas_controller;

	wxListBox* m_lb_models;

	VectorCtrl* m_vct_position;
	VectorCtrl* m_vct_rotation;
	VectorCtrl* m_vct_scale;

	wxStaticText* m_stxt_position;
	wxStaticText* m_stxt_rotation;
	wxStaticText* m_stxt_scale;

	wxRadioBox* m_rdobx_render_mode;

	void lb_models_OnSelection(wxCommandEvent& evt);
	void lb_models_OnChar(wxKeyEvent& evt);
	void rdobx_render_mode_OnChanged(wxCommandEvent& evt);

	void vct_position_OnChange(VectorCtrlEvent& evt);
	void vct_rotation_OnChange(VectorCtrlEvent& evt);
	void vct_scale_OnChange(VectorCtrlEvent& evt);

	void Mainloop(wxIdleEvent& evt);

	void HandleConnectionEvent(ConnectionEvent& evt);

public:
	Main();

	void SetModel(std::shared_ptr<Model> model);

	void ReloadSettings();
	nlohmann::json& GetSettings();
	const nlohmann::json& GetSettings() const;

	const std::shared_ptr<EngineConnection>& GetConnection() const;
	void ConnectTo(std::string address, int port);
	bool IsConnected() const;
};