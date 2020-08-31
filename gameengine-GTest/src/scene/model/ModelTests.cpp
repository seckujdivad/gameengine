#include "pch.h"

#include "scene/model/Model.h"

TEST(ModelTests, WireframeColour_SetGet)
{
	Model* mdl = new Model(0, ModelGeometry());

	mdl->SetWireframeColour(glm::vec3(0.0f, 0.5f, 1.0f));

	EXPECT_EQ(mdl->GetWireframeColour(), glm::vec3(0.0f, 0.5f, 1.0f));
}