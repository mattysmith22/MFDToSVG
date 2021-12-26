params["_config"];
private _properties = [];
private _subclasses = [];
{
	if (isClass _x) then {
		_subclasses pushBack [configName _x, _x call mbs_fnc_exportconfig];
	} else {
		if (isNumber _x) then {
			_properties pushBack [configName _x, getNumber _x];
		} else {
			if (isText _x) then {
				_properties pushBack [configName _x, getText _x];
			} else {
				_properties pushBack [configName _x, getArray _x];
			}
		}
	}
} foreach configProperties [_config,"true",true];
[_properties, _subclasses]