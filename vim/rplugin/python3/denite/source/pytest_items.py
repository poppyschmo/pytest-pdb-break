from denite.source.base import Base
from denite.kind.command import Kind as Command


class Source(Base):

    def __init__(self, vim):
        super().__init__(vim)
        self.name = "pytest_items"
        self.kind = Kind(vim)

    def gather_candidates(self, context):
        locs = context.get("_pytest_item_locations", [])
        return [{"word": l["nodeid"], "action__command": l} for l in locs]


class Kind(Command):
    def __init__(self, vim):
        super().__init__(vim)
        self.name = 'pytest_item'
