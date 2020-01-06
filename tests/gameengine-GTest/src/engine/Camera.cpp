/*
The second include shouldn't be necessary (and should stop the compile completely), but it seems to be.
If this is stopping a compile on another setup, just comment it out and ignore.
*/

#include <gtest/gtest.h>
#include "pch.h"


namespace CameraTests
{
	TEST(Identifier, Set)
	{
		EXPECT_EQ(1, 1);
	}
}