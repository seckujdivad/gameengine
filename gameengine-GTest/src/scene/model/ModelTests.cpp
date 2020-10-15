#include "pch.h"
#include "scene/model/Model.h"

TEST(ModelTests, WireframeColour_SetGet)
{
	Model* mdl = new Model(0, ModelGeometry());

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
}