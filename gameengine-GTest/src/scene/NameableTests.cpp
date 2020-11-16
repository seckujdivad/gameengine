#include "pch.h"
#include "scene/Nameable.h"

TEST(NameableTests, Identifier_SetGet)
{
	Nameable* nameable = new Nameable();
	EXPECT_EQ(nameable->GetIdentifier(), "");

	nameable->SetIdentifier("Hello World!");
	EXPECT_EQ(nameable->GetIdentifier(), "Hello World!");

	delete nameable;
}