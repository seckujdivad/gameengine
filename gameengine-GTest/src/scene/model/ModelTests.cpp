#include "pch.h"

#include "scene/model/Model.h"

TEST(ModelTests, Vertex_SetGet_Vector)
{
	Model* mdl = new Model();
	
	std::vector<GLfloat> vertices = { 0.1f, 3.5f, -9.0f };
	int index = mdl->AddVertex(vertices);
	std::vector<GLfloat>* vertices_result = mdl->GetVertex(index);

	for (size_t i = 0; i < 3; i++)
	{
		EXPECT_EQ(vertices_result->at(i), vertices.at(i));
	}
}