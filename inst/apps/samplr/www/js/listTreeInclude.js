/*globals console, changeBrowseGermplasmButtonBehavior, chooseListNode, chooseList, showListTreeToolTip, hasChildrenString, cannotMove, pleaseChooseFolder*/
/*glboals getMessageErrorDiv, showErrorMessage, moveGermplasm, moveSamplesListFolder*/
/*exported displayGermplasmListTree, displaySampleListTree, openTreeType*/
/*exported getDisplayedTreeName, doGermplasmLazyLoad, doSampleLazyLoad*/

var lazyLoadUrl = '/Fieldbook/ListTreeManager/expandGermplasmTree/',
	additionalLazyLoadUrl = '',
	germplasmFocusNode = null,
	sampleFocusNode = null,
	openTreeType = 0,
	selectListFunction;

function getDisplayedTreeName() {
	'use strict';
	if ($('#listTreeModal').hasClass('in') === true) {
		return 'germplasmTree';
	} else if ($('#saveListTreeModal').hasClass('in') === true) {
		return 'germplasmFolderTree';
	} else if ($('#saveSampleListTreeModal').hasClass('in') === true) {
		return 'sampleFolderTree';
	}
}
function getDisplayedModalSelector() {
	'use strict';
	if ($('#listTreeModal').hasClass('in') === true) {
		return '#listTreeModal';
	} else if ($('#saveListTreeModal').hasClass('in') === true) {
		return '#saveListTreeModal';
	} else if ($('#saveSampleListTreeModal').hasClass('in') === true) {
		return '#saveSampleListTreeModal';
	}
}
function getMessageErrorDiv() {
	'use strict';
	var msgDiv = 'page-import-message-modal';
	if (getDisplayedTreeName() === 'germplasmFolderTree' || getDisplayedTreeName() === 'sampleFolderTree') {
		msgDiv = 'page-save-list-message-modal';
	}
	return msgDiv;
}

function doGermplasmLazyLoad(node) {
	'use strict';

	if (node.data.isFolder === true) {

		node.appendAjax({
			url : lazyLoadUrl + node.data.key + additionalLazyLoadUrl,
			dataType : 'json',
			async : false,
			success : function(node) {
				//do nothing

				setTimeout(function() {
					node.focus();
				}, 50);
				if (node.hasChildren()) {
					$('.delete-germplasm-folder').addClass(
						'disable-image');
				}

				if (node.data.isFolder === false) {
					changeBrowseGermplasmButtonBehavior(false);
				} else {



					if (node.data.key === 'LISTS' || node.data.key === 'CROPLISTS') {
						changeBrowseGermplasmButtonBehavior(true);
						$('.edit-germplasm-folder').addClass(
							'disable-image');
						$('.delete-germplasm-folder').addClass(
							'disable-image');
						if (node.data.key === 'CROPLISTS') {
							$('.create-germplasm-folder').addClass(
								'disable-image');
						}
					} else {
						changeBrowseGermplasmButtonBehavior(true);
					}
				}
			},
			error : function(node, XMLHttpRequest, textStatus,
							 errorThrown) {

				console.log('The following error occured: '
					+ textStatus, errorThrown);
			},
			cache : false
		});
		node.expand();
	}
}

function doSampleLazyLoad(node) {
	'use strict';

	if (node.data.isFolder === true) {
		var url = '/bmsapi/crops/' + cropName + '/sample-lists/tree?onlyFolders=0&parentFolderId=' + node.data.key;
		if (selectedProjectId) {
			url += '&programUUID=' + currentProgramId;
		}

		var xAuthToken = JSON.parse(localStorage["bms.xAuthToken"]).token;

		node.appendAjax({
			url: url,
			dataType : 'json',
			async : false,
			beforeSend: function (xhr) {
				xhr.setRequestHeader('X-Auth-Token', xAuthToken);
			},
			success : function(node) {
				//do nothing

				setTimeout(function() {
					node.focus();
				}, 50);
				if (node.hasChildren()) {
					$('.delete-germplasm-folder').addClass(
						'disable-image');
				}

				if (node.data.isFolder === false) {
					changeBrowseSampleButtonBehavior(false);
				} else {
					if (node.data.key === 'LISTS' || node.data.key === 'CROPLISTS') {
						changeBrowseSampleButtonBehavior(true);
						$('.edit-germplasm-folder').addClass(
							'disable-image');
						$('.delete-germplasm-folder').addClass(
							'disable-image');
						if (node.data.key === 'CROPLISTS') {
							$('.create-germplasm-folder').addClass(
								'disable-image');
						}
					} else {
						changeBrowseSampleButtonBehavior(true);
					}
				}
			},
			error : function(node, XMLHttpRequest, textStatus,
							 errorThrown) {

				console.log('The following error occured: '
					+ textStatus, errorThrown);
			},
			cache : false
		});
		node.expand();
	}
}

/** Germplasm Tree **/
function displayGermplasmListTree(treeName, isLocalOnly, isFolderOnly,
								  clickFunction) {
	'use strict';
	var lazyLoadUrlGetChildren = '/Fieldbook/ListTreeManager/expandGermplasmTree/';
	var initLoadUrl = '/Fieldbook/ListTreeManager/loadInitGermplasmTree';
	initLoadUrl += '/' + isFolderOnly;

	var dynaTreeOptions = {
		title : treeName,
		checkbox : false,
		noLink : false,
		autoFocus : false,
		imagePath : '/Fieldbook/static/img/',
		activeVisible : true,
		initAjax : {
			url : initLoadUrl,
			dataType : 'json'
		},
		onLazyRead : function(node) {
			doGermplasmLazyLoad(node);
		},
		onRender : function(node, nodeSpan) {
			if (node.data.key !== 'LISTS'
				&& node.data.key !== 'CROPLISTS'
				&& node.data.key !== '_statusNode'
				&& node.data.isFolder === false) {
				//showListTreeToolTip(node, nodeSpan);
				$(nodeSpan)
					.find('a.dynatree-title')
					.hover(
						function() {
							if ($(nodeSpan).find(
									'a.dynatree-title')
									.hasClass('has-popover') === false) {
								$(nodeSpan)
									.find(
										'a.dynatree-title')
									.addClass('has-popover');
								showListTreeToolTip(node,
									nodeSpan);
							}

						});
			}
		},
		classNames : {
			container : 'fbtree-container',
			expander : 'fbtree-expander',
			nodeIcon : 'fbtree-icon',
			combinedIconPrefix : 'fbtree-ico-',
			focused : 'fbtree-focused',
			active : 'fbtree-active',
			nodeLoading : ''
		},
		onFocus : function(node) {
			var nodeSpan = node.span;
			if (node.data.key !== 'LISTS'
				&& node.data.key !== 'CROPLISTS'
				&& node.data.key !== '_statusNode'
				&& node.data.isFolder === false) {
				$(nodeSpan)
					.find('a.dynatree-title')
					.hover(
						function() {
							if ($(nodeSpan).find(
									'a.dynatree-title')
									.hasClass('has-popover') === false) {
								$(nodeSpan)
									.find(
										'a.dynatree-title')
									.addClass('has-popover');
								showListTreeToolTip(node,
									nodeSpan);
							}
						});
				if ($(nodeSpan).find('a.dynatree-title').hasClass(
						'has-popover') === false) {
					$(nodeSpan).find('a.dynatree-title').addClass(
						'has-popover');
					showListTreeToolTip(node, nodeSpan);
				} else {
					$('.popover').hide();
					$(nodeSpan).find('a.dynatree-title')
						.popover('show');
				}
			}
			germplasmFocusNode = node;
		},
		onActivate : function(node) {

			if (node.data.isFolder === false) {
				changeBrowseGermplasmButtonBehavior(false);
			} else {
				if (node.data.key === 'LISTS' || node.data.key === 'CROPLISTS') {
					changeBrowseGermplasmButtonBehavior(true);
					$('.edit-germplasm-folder').addClass(
						'disable-image');
					$('.delete-germplasm-folder').addClass(
						'disable-image');
					if ( node.data.key === 'CROPLISTS') {
						$('.create-germplasm-folder').addClass(
							'disable-image');
					}
				} else {
					changeBrowseGermplasmButtonBehavior(true);
				}
			}
			$('.germplasm-tree-section a.dynatree-title').off('keyup');
			$('.germplasm-tree-section a.dynatree-title').on('keyup',
				function(e) {
					if (e.keyCode === 13) {
						chooseListNode(germplasmFocusNode, true);
					}
				});

			$(node.span).trigger('bms.tree.node.activate');
		},
		onDblClick : function(node, event) {
			chooseList();
		},
		dnd : {
			onDragStart : function(node) {
				return true;
			},
			preventVoidMoves : true, // Prevent dropping nodes 'before self', etc.
			onDragEnter : function(node, sourceNode) {
				return true;
			},
			onDragOver : function(node, sourceNode, hitMode) {

			},
			onDrop : function(node, sourceNode, hitMode, ui, draggable) {
				/** This function MUST be defined to enable dropping of items on
				 * the tree.
				 */
				if (sourceNode.hasChildren()) {
					showErrorMessage(getMessageErrorDiv(), cannotMove
						+ ' ' + sourceNode.data.title + ' '
						+ hasChildrenString);
				} else if (node.data.key === 'CROPLISTS' && sourceNode.data.isFolder) {
					showErrorMessage(getMessageErrorDiv(), cannotMoveFolderToCropListError);
                } else if (!node.data.isFolder) {
                    showErrorMessage(getMessageErrorDiv(), cannotMoveItemToAListError);
				} else {
					$.ajax({
						url : lazyLoadUrlGetChildren
						+ sourceNode.data.key,
						type : 'GET',
						cache : false,
						aysnc : false,
						success : function(data) {
							var childCount = $.parseJSON(data).length;
							if (childCount === 0) {
								moveGermplasm(sourceNode, node);
							} else {
								showErrorMessage(getMessageErrorDiv(),
									cannotMove + ' '
									+ sourceNode.data.title
									+ ' '
									+ hasChildrenString);
							}

						}
					});
				}
			},
			onDragLeave : function(node, sourceNode) {
				/** Always called if onDragEnter was called.
				 */
			}
		}
	};

	if (clickFunction) {
		dynaTreeOptions.onClick = clickFunction;
	}

	$('#' + treeName).dynatree(dynaTreeOptions);
}


// TODO Extract common functionality with displayGermplasmListTree
/** Sample Tree **/
function displaySampleListTree(treeName, isLocalOnly, isFolderOnly,
							   clickFunction) {
	'use strict';
	var lazyLoadUrlGetChildren = '/bmsapi/crops/' + cropName + '/sample-lists/tree?onlyFolders=0';
	var initLoadUrl ='/bmsapi/crops/' + cropName + '/sample-lists/tree?onlyFolders=' + isFolderOnly;
	if (selectedProjectId) {
		lazyLoadUrlGetChildren += '&programUUID=' + currentProgramId;
		initLoadUrl += '&programUUID=' + currentProgramId;
	}

	var xAuthToken = JSON.parse(localStorage["bms.xAuthToken"]).token;

	var dynaTreeOptions = {
		title : treeName,
		checkbox : false,
		noLink : false,
		autoFocus : false,
		imagePath : '/Fieldbook/static/img/',
		activeVisible : true,
		initAjax : {
			url : initLoadUrl,
			dataType : 'json',
			beforeSend: function (xhr) {
				xhr.setRequestHeader('X-Auth-Token', xAuthToken);
			}
		},
		onLazyRead : function(node) {
			doSampleLazyLoad(node);
		},
		onRender : function(node, nodeSpan) {
			if (node.data.key !== 'LISTS'
				&& node.data.key !== 'CROPLISTS'
				&& node.data.key !== '_statusNode'
				&& node.data.isFolder === false) {
				$(nodeSpan)
					.find('a.dynatree-title')
					.hover(
						function() {
							if ($(nodeSpan).find(
									'a.dynatree-title')
									.hasClass('has-popover') === false) {
								$(nodeSpan)
									.find(
										'a.dynatree-title')
									.addClass('has-popover');
							}
						});
			}
		},
		classNames : {
			container : 'fbtree-container',
			expander : 'fbtree-expander',
			nodeIcon : 'fbtree-icon',
			combinedIconPrefix : 'fbtree-ico-',
			focused : 'fbtree-focused',
			active : 'fbtree-active',
			nodeLoading : ''
		},
		onFocus : function(node) {
			var nodeSpan = node.span;
			if (node.data.key !== 'LISTS'
				&& node.data.key !== 'CROPLISTS'
				&& node.data.key !== '_statusNode'
				&& node.data.isFolder === false) {
				$(nodeSpan)
					.find('a.dynatree-title')
					.hover(
						function() {
							if ($(nodeSpan).find(
									'a.dynatree-title')
									.hasClass('has-popover') === false) {
								$(nodeSpan)
									.find(
										'a.dynatree-title')
									.addClass('has-popover');
							}
						});
				if ($(nodeSpan).find('a.dynatree-title').hasClass(
						'has-popover') === false) {
					$(nodeSpan).find('a.dynatree-title').addClass(
						'has-popover');
				} else {
					$('.popover').hide();
					$(nodeSpan).find('a.dynatree-title')
						.popover('show');
				}
			}
			sampleFocusNode = node;
		},
		onActivate : function(node) {
			if (node.data.isFolder === false) {
				changeBrowseSampleButtonBehavior(false);
			} else {
				if (node.data.key === 'LISTS' || node.data.key === 'CROPLISTS') {
					changeBrowseSampleButtonBehavior(true);
					$('.edit-germplasm-folder').addClass(
						'disable-image');
					$('.delete-germplasm-folder').addClass(
						'disable-image');
					if ( node.data.key === 'CROPLISTS') {
						$('.create-germplasm-folder').addClass(
							'disable-image');
					}
				} else {
					changeBrowseSampleButtonBehavior(true);
				}
			}
			$('.germplasm-tree-section a.dynatree-title').off('keyup');
			$('.germplasm-tree-section a.dynatree-title').on('keyup',
				function(e) {
					if (e.keyCode === 13) {
						chooseListNode(sampleFocusNode, true);
					}
				});
			$(node.span).trigger('bms.tree.node.activate');
		},
		onDblClick : function(node, event) {
			chooseList(node);
		},
		dnd : {
			onDragStart : function(node) {
				return true;
			},
			preventVoidMoves : true, // Prevent dropping nodes 'before self', etc.
			onDragEnter : function(node, sourceNode) {
				return true;
			},
			onDragOver : function(node, sourceNode, hitMode) {

			},
			onDrop : function(node, sourceNode, hitMode, ui, draggable) {
				/** This function MUST be defined to enable dropping of items on
				 * the tree.
				 */
				if (sourceNode.hasChildren()) {
					showErrorMessage(getMessageErrorDiv(), cannotMove
						+ ' ' + sourceNode.data.title + ' '
						+ hasChildrenString);
				} else if (node.data.key === 'CROPLISTS' && sourceNode.data.isFolder) {
					showErrorMessage(getMessageErrorDiv(), cannotMoveFolderToCropListError);
				} else {
					if (sourceNode.data.isFolder) {
						$.ajax({
							url : lazyLoadUrlGetChildren + '&parentFolderId=' + sourceNode.data.key,
							type : 'GET',
							cache : false,
							async : false,
							beforeSend: function (xhr) {
								xhr.setRequestHeader('X-Auth-Token', xAuthToken);
							},
							success : function(data) {
								var childCount = data.length;
								if (childCount === 0) {
									moveSamplesListFolder(sourceNode, node).done(function () {
										sourceNode.remove();
										doSampleLazyLoad(node);
										node.focus();
									});
								} else {
									showErrorMessage(getMessageErrorDiv(),
										cannotMove + ' '
										+ sourceNode.data.title
										+ ' '
										+ hasChildrenString);
								}

							}
						});
					} else {
						moveSamplesListFolder(sourceNode, node).done(function () {
							sourceNode.remove();
							doSampleLazyLoad(node);
							node.focus();
						});
					}
				}
			},
			onDragLeave : function(node, sourceNode) {
				/** Always called if onDragEnter was called.
				 */
			}
		}
	};

	if (clickFunction) {
		dynaTreeOptions.onClick = clickFunction;
	}

	$('#' + treeName).dynatree(dynaTreeOptions);
}
