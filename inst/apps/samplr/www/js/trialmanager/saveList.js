(function () {
	'use strict';

	var manageTrialApp = angular.module('manageTrialApp');

	manageTrialApp.controller('SaveListCtrl',
		['$rootScope', '$state', '$stateParams', '$scope', function ($rootScope, $state, $stateParams, $scope) {

			$scope.saveGermplasmList = function () {
				var chosenNodeFolder = $('#' + getDisplayedTreeName()).dynatree('getTree').getActiveNode();
				var errorMessageDiv = 'page-save-list-message-modal';
				if (chosenNodeFolder === null) {
					showErrorMessage(errorMessageDiv, listParentFolderRequired);
					return false;
				}
				if ($('#listName').val() === '') {
					showInvalidInputMessage(listNameRequired);
					return false;
				}
				if ($('#listType').val() === '') {
					showInvalidInputMessage(listTypeRequired);
					return false;
				}
				if ($('#listDate').val() === '') {
					showInvalidInputMessage(listDateRequired);
					return false;
				}
				var invalidDateMsg = validateAllDates();
				if (invalidDateMsg !== '') {
					showInvalidInputMessage(invalidDateMsg);
					return false;
				}

				var parentId = chosenNodeFolder.data.key;
				$('#saveListForm #parentId').val(parentId);

				var saveList = '/Fieldbook/ListTreeManager/saveList/';
				var isCrosses = false;

				if ($('#saveListTreeModal').data('is-save-crosses') === '1') {
					isCrosses = true;
				}


				if (isCrosses) {
					$('#germplasmListType').val('cross');
				} else {
					$('#germplasmListType').val('advance');
				}

				var dataForm = $('#saveListForm').serialize();

				//TODO add error handler
				$.ajax({
					url: saveList,
					type: 'POST',
					data: dataForm,
					cache: false,
					success: function (data) {
						if (data.isSuccess === 1) {
							$('#saveListTreeModal').modal('hide');
							if (isCrosses) {
								$('#saveListTreeModal').data('is-save-crosses', '0');
								if (data.isTrimed === 1) {
									showAlertMessage('page-save-list-message-modal', crossesWarningMessage, 10000);
								}
							} else if (data.isNamesChanged === 1) {
									showAlertMessage('page-save-list-message-modal', namesChangedWarningMessage, 10000);
							}
							showSuccessfulMessage('', saveListSuccessfullyMessage);




							// Notify the application that germplasm has been saved. This will display the 'Crosses and Selections'
							// tab if germplasm is already created within the study.
							$rootScope.$broadcast('germplasmListSaved');

							// Refresh and show the 'Crosses and Selections' tab after saving the germplasm list
							$rootScope.navigateToTab('germplasmStudySource', {reload: true});


						} else {
							showErrorMessage('page-save-list-message-modal', data.message);
						}
					}
				});
			};

		}]);
})();