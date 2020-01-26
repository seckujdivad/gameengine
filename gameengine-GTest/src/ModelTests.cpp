#include "pch.h"
#include "Model.h"

TEST(ModelTests, Identifier_GetSet)
{
	Model* mdl = new Model();
	EXPECT_EQ(mdl->GetIdentifier(), "");

	mdl->SetIdentifier("Hello World!");
	EXPECT_EQ(mdl->GetIdentifier(), "Hello World!");
}