#include <wx/wxprec.h>
#include "SvgLoader.h"

void UVFromSvg(Model* model, std::string path)
{
	wxXmlDocument xml_doc;
	if (!xml_doc.Load(path))
	{
		throw std::runtime_error("Couldn't open file '" + path + '"');
	}
}