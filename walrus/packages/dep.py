class Dep:
    name = ""
    url = ""
    vsn = ""

    def __init__(self, name, url, vsn):
        self.name = name
        self.url = url
        self.vsn = vsn

    @classmethod
    def fromjson(cls, json):
        return cls(json['name'], json['url'], json['vsn'])

    def export(self):
        return {'name': self.name,
                'url': self.url,
                'vsn': self.vsn}

    @staticmethod
    def export_all(deps):
        return [dep.export() for dep in deps]
