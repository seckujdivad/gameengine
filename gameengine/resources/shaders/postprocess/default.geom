#version 430 core
layout (triangles) in;
layout (triangle_strip, max_vertices = 18) out;

in vec2 vertUV[];
in vec4 vertPos[];

out vec2 geomUV;

uniform bool is_cubemap;

void set_outputs(int index)
{
	geomUV = vertUV[index];
	gl_Position = vertPos[index];
}

void main()
{
	int num_layers = is_cubemap ? 6 : 1;

	for (int layer = 0; layer < num_layers; layer++)
	{
		gl_Layer = layer;
		for (int i = 0; i < 3; i++)
		{
			set_outputs(i);
			EmitVertex();
		}
		EndPrimitive();
	}
}