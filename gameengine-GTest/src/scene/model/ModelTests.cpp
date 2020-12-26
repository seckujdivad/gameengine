#include "pch.h"

#include <vector>
#include <memory>

#include "scene/model/Model.h"
#include "scene/model/geometry/Polygonal.h"

TEST(ModelTests, WireframeColour_SetGet)
{
	std::vector<std::shared_ptr<Geometry>> geometry({ std::make_shared<Polygonal>() });
	Model* mdl = new Model(0, geometry);

	mdl->SetWireframeColours({
		glm::vec3(0.0f),
		glm::vec3(1.0f),
		glm::vec3(1.0f, 0.5f, 0.25f)
		});

	EXPECT_EQ(mdl->GetCurrentWireframeColour(), glm::vec3(0.0f));
	
	mdl->SetCurrentWireframeColour(glm::vec3(0.0f, 1.0f, 0.0f));
	EXPECT_EQ(mdl->GetCurrentWireframeColour(), glm::vec3(0.0f, 1.0f, 0.0f));

	mdl->SetCurrentWireframeIndex(1);
	EXPECT_EQ(mdl->GetCurrentWireframeColour(), glm::vec3(1.0f));

	mdl->SetCurrentWireframeIndex(2);
	EXPECT_EQ(mdl->GetCurrentWireframeColour(), glm::vec3(1.0f, 0.5f, 0.25f));

	mdl->SetCurrentWireframeIndex(0);
	EXPECT_EQ(mdl->GetCurrentWireframeColour(), glm::vec3(0.0f, 1.0f, 0.0f));

	delete mdl;
}