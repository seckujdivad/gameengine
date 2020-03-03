#version 400 core
layout(location = 0) out vec4 frag_out;
in vec3 globalPos;
in vec3 globalNormal;
in vec2 globalUV;

uniform sampler2D colourTexture;

uniform vec3 light_ambient;

void main()
{
	//frag_out = vec4(normalize(globalPos), 1.0f);
	//frag_out = vec4(vec3(gl_FragCoord.z).xyz, 1.0f);
	//frag_out = vec4(globalUV.st, 0.0f, 1.0f);

	//get base colour
	frag_out = texture(colourTexture, globalUV);
	
	//calculate light intensity
	vec3 frag_intensity = vec3(0.0f);
	frag_intensity = frag_intensity + light_ambient;

	//frag_out = vec4(frag_out.x * frag_intensity.x, frag_out.y * frag_intensity.y, frag_out.z * frag_intensity.z, frag_out.w);
	frag_out = vec4(frag_intensity, 1.0f) * frag_out;
}