// Generic component that provides treetable functionality
// TODO Once a draggable approach is found, implement in angularjs for studies and angular 2 for new components
var TreeTable = (function () {

	var self; // FIXME self is static here. Rewrite to allow multiple copies of treetable
	var resolved = $.Deferred().resolve().promise();


	/**
	 * Constructor
	 * @param options:
	 * - baseurl
	 * - treeTableSelector
	 * - tableContainer: element to insert the table
	 * - context: jquery context
	 * - authparams: send as data to ajax requests
	 * - crop
	 * - move function to move elements in the table
	 * - rename function to rename elements in the table
	 * - create function to create elements in the table
	 * - delete function to delete elements in the table
	 * - ondblclick callback for node double click
	 * @constructor
	 */
	function TreeTable(options) {
		this.baseurl = options.baseurl;
		this.treeTableSelector = options.treeTableSelector;
		this.tableContainer = options.tableContainer;
		this.authparams = options.authparams;
		this.context = options.context;
		this.crop = options.crop;
		this.move = options.move;
		this.rename = options.rename;
		this.ondblclick = options.ondblclick;
		this.create = options.create;
		this.delete = options.delete;
		self = this;
	}

	// public

	TreeTable.prototype.init = function (id) {
		return $.ajax({
			url: self.baseurl + '/loadInitTreeTable',
			data: self.authparams,
			type: 'GET',
			cache: false
		}).done(function (html) {
			self.tableContainer.html(html);
			self.table = $(self.treeTableSelector);
			// TODO TreePersist.preLoadTreeState
			self.table.treetable({
				expandable: true,
				clickableNodeNames: true,
				onNodeCollapse: function () {
					self.table.treetable("unloadBranch", this);
				},
				onNodeExpand: function () {
					expandListNode(this);
				},
				onInitialized: function () {
					initializeListTreeTable();
				}
			});
		});
	};

	TreeTable.prototype.expandListInTreeTable = function (id) {
		self.table.treetable('expandNode', id);
	};

	TreeTable.prototype.addFolderInTreeTable = function(elem) {
		'use strict';
		if (!$(elem).hasClass('disable-image')) {
			self.hideFolderDiv('#renameFolderDiv');
			$('#addFolderName', self.context).val('');
			$('#addFolderDiv', self.context).slideDown('fast');
		}
	};

	TreeTable.prototype.createFolderInTreeTable = function() {
		'use strict';

		if (!self.getSelectedList()) {
			showErrorMessage('page-rename-folder-message-modal', invalidNodeTreeMessage);
			return resolved;
		}

		var folderName = $.trim($('#addFolderName', self.context).val()),
			parentFolderId;

		if (folderName === '') {
			showErrorMessage('page-add-folder-message-modal', folderNameRequiredMessage);
			return resolved;
		} else if (!isValidInput(folderName)) {
			showErrorMessage('page-add-folder-message-modal', invalidFolderNameCharacterMessage);
			return resolved;
		} else {
			parentFolderId = self.getSelectedListId();
			if (parentFolderId === 'LISTS' || parentFolderId === 'CROPLISTS') {
				parentFolderId = 0;
			}

			var id;
			return self.create(parentFolderId, folderName)
				.then(function (data) {
					id = data.id;
					return expandListNode(self.table.treetable('node', getParentOfNewlyAddedList()));
				})
				.done(function () {
					selectNode(id);
					setAsDroppableAndDraggable(id);
					self.hideFolderDiv('#addFolderDiv');
					changeBrowseButtonBehavior(true);
					showSuccessfulMessage('', addFolderSuccessful);
				});
		}
		return resolved;
	};

	TreeTable.prototype.renameFolderInTreeTable = function(elem) {
		'use strict';

		var currentFolderName;

		if (!$(elem).hasClass('disable-image')) {
			self.hideFolderDiv('#addFolderDiv');
			currentFolderName = getSelectedListName();
			$('#newFolderName', self.context).val($.trim(currentFolderName));

			$('#renameFolderDiv', self.context).slideDown('fast');
		}
	};

	TreeTable.prototype.submitRenameFolderInTreeTable = function() {
		'use strict';

		var folderName = $.trim($('#newFolderName', self.context).val());

		if (!self.getSelectedList()) {
			showErrorMessage('page-rename-folder-message-modal', invalidNodeTreeMessage);
			return resolved;
		}

		var id = self.getSelectedListId();

		if (id === 'LISTS') {
			showErrorMessage('page-rename-folder-message-modal', renameInvalidFolderMessage);
			return resolved;
		}

		var name = getSelectedListName();
		if (folderName === $.trim(name)) {
			self.hideFolderDiv('#renameFolderDiv');
			return resolved;
		}

		if (folderName === '') {
			showErrorMessage('page-rename-folder-message-modal', folderNameRequiredMessage);
			return resolved;
		} else if (!isValidInput(folderName)) {
			showErrorMessage('page-rename-folder-message-modal', invalidFolderNameCharacterMessage);
			return resolved;
		} else {
			return self.rename(id, folderName)
				.done(function (data) {
					self.hideFolderDiv('#renameFolderDiv');
					setSelectedListName(folderName);
					showSuccessfulMessage('', renameItemSuccessful);
				});
		}
	};

	TreeTable.prototype.deleteFolderInTreeTable = function(elem) {
		'use strict';

		var currentFolderName;

		if (!$(elem).hasClass('disable-image')) {
			$('#deleteFolder', self.context).modal('show');
			$('#addFolderDiv', self.context).slideUp('fast');
			$('#renameFolderDiv', self.context).slideUp('fast');
			currentFolderName = getSelectedListName();
			$('#delete-folder-confirmation', self.context).html(deleteConfirmation + ' ' + currentFolderName + '?');

			$('#page-delete-folder-message-modal', self.context).html('');
		}
	};

	TreeTable.prototype.submitDeleteFolderInTreeTable = function() {
		'use strict';

		var folderId = self.getSelectedListId();

		return self.delete(folderId)
			.done(function () {
				$('#deleteFolder', self.context).modal('hide');
				var node = self.getSelectedList();
				node.remove();
				updateTableRowsBgColor();
				changeBrowseButtonBehavior(false);
				showSuccessfulMessage('', deleteItemSuccessful);
			});
	};

	TreeTable.prototype.hideFolderDiv = function(selector) {
		$(selector, self.context).slideUp('fast');
	};

	TreeTable.prototype.getSelectedList = function() {
		return $('tr.selected', self.table);
	}

	TreeTable.prototype.getSelectedListId = function() {
		return $('tr.selected', self.table).attr('data-tt-id');
	}

	TreeTable.prototype.isSelectedListAFolder = function() {
		return $('tr.selected', self.table).attr('is-folder') === '1';
	}

	// private

	function changeBrowseButtonBehavior(enable) {
		if (enable) {
			$('.browse-action', self.context).removeClass('disable-image');
		} else {
			$('.browse-action', self.context).addClass('disable-image');
		}
	}

	function initializeListTreeTable() {
		$('.list-row', self.context).each(function (index) {
			if ($(this).attr('num-of-children') === '0') {
				$(this).attr('data-tt-branch', false);
			} else {
				$(this).attr('data-tt-branch', true);
			}
		});
		changeBrowseButtonBehavior(false);
		updateTools();
		updateDraggableTableRows();
		updateDroppableTableRows();
		self.table.treetable('unloadBranch', self.table.treetable('node', 'LISTS'));
		updateTableRowsBgColor();
		self.table.find('.file').each(function () {
			updateDoubleClickEvent($(this));
		});
	}

	function expandListNode(node) {
		if (typeof node === "undefined") {
			return;
		}
		return $.ajax({
			async: false,
			url: self.baseurl + '/expandGermplasmListFolder/' + node.id,
			data: self.authparams,
		}).done(function (html) {
			var rows = $(html).filter('tr');

			rows.filter(':has(.folder)').each(function () {
				updateExpandEventIfHasChildren($(this));
			});
			rows.find('.file').each(function () {
				updateDoubleClickEvent($(this));
			});
			self.table.treetable('unloadBranch', node);
			self.table.treetable('loadBranch', node, rows);

			updateTableRowsBgColor();
			updateTools();
			updateDroppableTableRows();
			updateDraggableTableRows();
		});
	}

	function updateExpandEventIfHasChildren(elem) {
		if (elem.attr('num-of-children') === '0') {
			elem.attr('data-tt-branch', false);
			elem.find('span.indenter').html('');
		} else {
			elem.attr('data-tt-branch', true);
		}
	}

	function updateDoubleClickEvent(elem) {
		elem.dblclick(function () {
			self.ondblclick(elem);
		});
	}

	function updateTableRowsBgColor() {
		$('.list-row', self.table).each(function (index) {
			$('td', $(this)).each(function () {
				$(this).removeClass('even');
				$(this).removeClass('odd');
				if (index % 2 === 0) {
					$(this).addClass('even');
				} else {
					$(this).addClass('odd');
				}
			});
		});
	}

	function updateTools() {
		self.table.find('tbody').off('mousedown', 'tr').on('mousedown', 'tr', function () {
			$('tr.selected', self.table).removeClass('selected');
			$(this).addClass('selected');
			changeBrowseButtonBehavior(true);
			if ($(this).attr('data-tt-id') === 'LISTS' || $(this).attr('data-tt-id') === 'CROPLISTS') {
				$('.edit-folder', self.context).addClass('disable-image');
				$('.delete-folder', self.context).addClass('disable-image');
				if ($(this).attr('data-tt-id') === 'CROPLISTS') {
					$('.create-folder', self.context).addClass('disable-image');
					self.hideFolderDiv('#addFolderDiv');
				}
				self.hideFolderDiv('#renameFolderDiv');
			} else if ($(this).attr('num-of-children') !== '0') {
				$('.delete-folder', self.context).addClass('disable-image');
			}
		});
	}

	function updateDraggableTableRows() {
		self.table.find('.folder, .file').each(function () {
			var rowId = $(this).parents('tr').attr('data-tt-id');
			if (rowId !== 'LISTS') {
				treeTableDraggableSetup.apply(this);
			}
		});
	}

	function treeTableDraggableSetup() {
		$(this).draggable({
			helper: 'clone',
			opacity: .75,
			refreshPositions: true,
			revert: 'invalid',
			revertDuration: 300,
			scroll: true
		});
	}

	function updateDroppableTableRows() {
		self.table.find('.folder').parents('tr').each(function () {
			treeTableDroppableSetup.apply(this);
		});
	}

	function treeTableDroppableSetup() {
		$(this).droppable({
			accept: '.file, .folder',
			drop: function (e, ui) {
				var droppedEl, sourceNode, targetNode;
				droppedEl = ui.draggable.parents('tr');
				sourceNode = self.table.treetable('node', droppedEl.data('ttId'));
				var sourceNodeObj = $('tr[data-tt-id=' + sourceNode.id + ']', self.table);
				var sourceParentId = sourceNodeObj.attr('data-tt-parent-id');
				var sourceNodeName = sourceNodeObj.find('.name').first().text();

				// TODO This validation should be in the server
				var numOfChildren = sourceNodeObj.attr('num-of-children');
				if (numOfChildren !== '0') {
					showErrorMessage('', cannotMove + ' ' + sourceNodeName + ' ' + hasChildrenString);
					return false;
				}

				targetNode = self.table.treetable('node', $(this).data('ttId'));
				if (sourceNodeObj.attr('is-folder') === '1' && targetNode.id === 'CROPLISTS') {
					showErrorMessage('', cannotMoveFolderToCropListError);
					return false;
				}
				self.move(sourceNode, targetNode, sourceParentId)
					.done(function () {
						moveCallBack(sourceNode, targetNode, sourceParentId)
					});
				;
			},
			hoverClass: 'accept',
			over: function (e, ui) {
				var droppedEl = ui.draggable.parents('tr');
				if (this != droppedEl[0] && !$(this).is('.expanded')) {
					self.table.treetable('expandNode', $(this).data('ttId'));
				}
			}
		});
	}

	function moveCallBack(sourceNode, targetNode, sourceParentId) {
		self.table.treetable('move', sourceNode.id, targetNode.id);
		updateTableRowsBgColor();

		var sourceNodeObj = $('tr[data-tt-id=' + sourceNode.id + ']', self.table);
		updateParentId(sourceNodeObj, targetNode.id);

		var targetNodeObj = $('tr[data-tt-id=' + targetNode.id + ']', self.table);
		incrementNumberOfChildren(targetNodeObj);
		updateExpandEventIfHasChildren(targetNodeObj);

		var sourceParentObj = $('tr[data-tt-id=' + sourceParentId + ']', self.table);
		decrementNumberOfChildren(sourceParentObj);
		updateExpandEventIfHasChildren(sourceParentObj);
	}

	function updateParentId(elem, parentId) {
		elem.attr('data-tt-parent-id', parentId);
	}

	function incrementNumberOfChildren(elem) {
		var numOfChildren = parseInt(elem.attr('num-of-children')) + 1;
		elem.attr('num-of-children', '' + numOfChildren);
	}

	function decrementNumberOfChildren(elem) {
		var numOfChildren = parseInt(elem.attr('num-of-children')) - 1;
		elem.attr('num-of-children', '' + numOfChildren);
	}

	function setAsDroppableAndDraggable(id) {
		$('tr[data-tt-id=' + id + ']', self.table).each(function () {
			treeTableDroppableSetup.apply(this);
		});
		$('tr[data-tt-id=' + id + '] .folder', self.table).each(function () {
			treeTableDraggableSetup.apply(this);
		});
	}

	function getParentOfNewlyAddedList() {
		var parentId = self.getSelectedListId();
		if (!self.isSelectedListAFolder()) {
			parentId = getSelectedListParentId();
		}
		return parentId;
	}

	function selectNode(id) {
		$('tr.selected', self.table).removeClass('selected');
		$('tr[data-tt-id=' + id + ']', self.table).addClass('selected');
	}

	function getSelectedListParentId() {
		return $('tr.selected', self.table).attr('data-tt-parent-id');
	}

	function getSelectedListName() {
		return $('tr.selected .name', self.table).text();
	}

	function setSelectedListName(name) {
		return $('tr.selected .name', self.table).text(name);
	}

	return TreeTable;
}())
