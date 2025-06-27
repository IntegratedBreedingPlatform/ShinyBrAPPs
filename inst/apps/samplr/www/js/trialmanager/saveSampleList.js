/*globals displaySampleListTree, changeBrowseSampleButtonBehavior,saveGermplasmReviewError */
/*globals $,showErrorMessage, showInvalidInputMessage, getDisplayedTreeName, listShouldNotBeEmptyError, getJquerySafeId,
 validateAllDates, saveSampleListSuccessfullyMessage */
/*globals listParentFolderRequired, listNameRequired */
/*globals listDateRequired, listTypeRequired, moveToTopScreen */
/*globals TreePersist, showSuccessfulMessage, console, ListTreeOperation */
/*exported save, openSaveSampleListModal*/

var SaveSampleList = {};

(function() {
	'use strict';

	SaveSampleList.toggleSaveButton = function () {
		var disableButton = $("#sampleListName").val() === ''
			|| $('#sampleListDate').val() === '';
		$("#submitSampleList").prop("disabled", disableButton);
	};

	SaveSampleList.initializeSampleListTree = function() {
		displaySampleListTree('sampleFolderTree', true, 1);
		$('#sampleFolderTree').off('bms.tree.node.activate').on('bms.tree.node.activate', function () {
			var id = $('#sampleFolderTree').dynatree('getTree').getActiveNode().data.key;
			if (id == 'CROPLISTS') {
				ListTreeOperation.hideFolderDiv('#addSampleFolderDiv');
				ListTreeOperation.hideFolderDiv('#renameSampleFolderDiv');
			}
		});
		changeBrowseSampleButtonBehavior(false);
		$('#saveSampleListTreeModal').off('hide.bs.modal');
		$('#saveSampleListTreeModal').on('hide.bs.modal', function() {
			TreePersist.saveSampleTreeState(false, '#sampleFolderTree');
			SaveSampleList.toggleSaveButton();
		});
		$('#saveSampleListTreeModal').on('hidden.bs.modal', function() {
			$('#sampleFolderTree').dynatree('getTree').reload();
			changeBrowseSampleButtonBehavior(false);
		});
	};

	SaveSampleList.openSaveSampleListModal = function(details) {
		SaveSampleList.details = details;
		$('#selectSelectionVariableToSampleListModal').modal('hide');

		var sampleListTreeNode = $('#sampleFolderTree').dynatree('getTree');
		setTimeout(function() {
			$('#saveSampleListTreeModal').on('shown.bs.modal', function () {
				$(getDisplayedModalSelector() + ' #renameSampleFolderDiv').slideUp('fast');
				$('#sampleListName').val('');
				$('#sampleListDate').datepicker({ dateFormat: 'yyyy-mm-dd'}).datepicker("setDate", new Date());
				$('#sampleListDescription').val('');
				$('#sampleListNotes').val('');
				if (SaveSampleList.details) {
                    $('#sampleListOwner').text(SaveSampleList.details.createdBy);
                }
				TreePersist.preLoadSampleTreeState(false, '#sampleFolderTree', true);
				$("#saveSampleListTreeModal .form-group").removeClass("has-error");
			});
			$('#saveSampleListTreeModal').modal({ backdrop: 'static', keyboard: true });
		}, 300);
		//we preselect the program lists
		if (sampleListTreeNode !== null && sampleListTreeNode.getNodeByKey('LISTS') !== null) {
			sampleListTreeNode.getNodeByKey('LISTS').activate();
		}
	};
	SaveSampleList.folderChangeKeypress = function (event, type){
		'use strict';
		if(event.keyCode === 13){
			if(type === '1'){
				ListTreeOperation.createSampleFolder();
			}else if(type === '2'){
				ListTreeOperation.submitRenameSampleListFolder();
			}
		}
	};

	SaveSampleList.save = function() {
		$("#saveSampleListTreeModal .form-group").removeClass("has-error");
		var chosenNodeFolder = $('#' + getDisplayedTreeName()).dynatree('getTree').getActiveNode();

		if (chosenNodeFolder === null) {
			Spinner.stop();
			showErrorMessage('page-save-list-message-modal', listParentFolderRequired);
			return false;
		}

		SaveSampleList.details.listName = $('#sampleListName').val();
		SaveSampleList.details.createdDate = $('#sampleListDate').val();
		SaveSampleList.details.description = $('#sampleListDescription').val();
		SaveSampleList.details.notes = $('#sampleListNotes').val();

		var parentId = chosenNodeFolder.data.key;
		var currentProgramUUID = SaveSampleList.details.programUUID;
		if (parentId === 'CROPLISTS') {
			SaveSampleList.details.programUUID = null;
		}

		if (parentId === 'LISTS' || parentId === 'CROPLISTS') {
			parentId = 0;
		}

		SaveSampleList.details.parentId = parentId;

		var xAuthToken = JSON.parse(localStorage["bms.xAuthToken"]).token;

		$("#submitSampleList").prop("disabled", true);
		Spinner.play();

		$.ajax({
			url: '/bmsapi/crops/' + cropName + '/sample-lists?programUUID=' + currentProgramUUID,
			type: 'POST',
			data: JSON.stringify(SaveSampleList.details),
			contentType: "application/json",
			beforeSend: function (xhr) {
				xhr.setRequestHeader('X-Auth-Token', xAuthToken);
			},
			error: function (data) {
				if (data.status == 401) {
					bmsAuth.handleReAuthentication();
				} else {
					showErrorMessage('page-save-list-message-modal', data.responseJSON.errors[0].message);
					if(data.responseJSON.errors[0].fieldNames == "ListName"){
						$('#sampleListName').closest(".form-group").addClass("has-error");
					}
				}
			},
			success: function (response) {
				showSuccessfulMessage('', saveSampleListSuccessfullyMessage);
				$('#saveSampleListTreeModal').modal('hide');
				displaySampleList(response.id, SaveSampleList.details.listName, false);
			},
			complete: function () {
				$("#submitSampleList").prop("disabled", false);
				Spinner.stop();
			}
		});
	};
})();

