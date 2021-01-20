#version 400 core
layout (triangles) in;
layout (triangle_strip, max_vertices = 18) out;

const int NUM_VERTICES = 3;

uniform bool is_cubemap;

void set_outputs(const int index)
{
}

void main()
{
	int num_layers = is_cubemap ? 6 : 1;

	for (int layer = 0; layer < num_layers; layer++)
	{
		gl_Layer = layer;

		for (int i = 0; i < NUM_VERTICES; i++)
		{
			set_outputs(i);
			EmitVertex();
		}
		EndPrimitive();
	}
}