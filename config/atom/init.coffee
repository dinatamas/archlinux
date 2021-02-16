# This is a simple fix that disables the auto-opening of the TreeView tab.
atom.workspace.onDidOpen ({item}) ->
  itemName = item.constructor.name
  if (itemName  != 'TreeView')
    dock = atom.workspace.paneContainerForURI('atom://tree-view')
    if dock && dock.isVisible()
      dock.hide()
