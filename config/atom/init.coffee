atom.workspace.onDidOpen ({item}) ->
  itemName = item.constructor.name
  if (itemName  != 'TreeView') # no action if open TreeView intentionally
    dock = atom.workspace.paneContainerForURI('atom://tree-view')
    if dock && dock.isVisible()
      dock.hide()