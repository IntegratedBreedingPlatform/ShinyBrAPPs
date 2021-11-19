(function () {
	'use strict';


	var createSampleModule = angular.module('create-sample', ['ui.bootstrap', 'datasets-api', 'datasetOptionModal', 'fieldbook-utils']);

	createSampleModule.factory('createSampleModalService', ['$uibModal',
		function ($uibModal) {

			var createSampleModalService = {};

			createSampleModalService.openDatasetOptionModal = function () {
				$uibModal.open({
					template: '<dataset-option-modal modal-title="modalTitle" message="message"' +
					'selected="selected" on-continue="next()"></dataset-option-modal>',
					controller: 'createSampleDatasetOptionCtrl',
					size: 'md'
				});
			};

			createSampleModalService.openSelectEnvironmentToSampleListModal = function (datasetId) {
				$uibModal.open({
					templateUrl: '/Fieldbook/static/angular-templates/createSample/selectEnvironmentToSampleListModal.html',
					controller: "selectEnvironmentToSampleListModalCtrl",
					size: 'md',
					resolve: {
						datasetId: function () {
							return datasetId;
						}
					},
					controllerAs: 'ctrl'
				});
			};

			createSampleModalService.openSelectSelectionVariableToSampleListModal = function (datasetId, instanceNumbers) {
				$uibModal.open({
					templateUrl: '/Fieldbook/static/angular-templates/createSample/selectSelectionVariableToSampleListModal.html',
					controller: "selectSelectionVariableToSampleListModalCtrl",
					size: 'md',
					resolve: {
						instanceNumbers: function () {
							return instanceNumbers;
						},
						datasetId: datasetId
					},
					controllerAs: 'ctrl'
				});
			};

			createSampleModalService.showAlertMessage = function (title, message) {
				// Call the global function to show alert message
				showAlertMessage(title, message);
			};

			createSampleModalService.showErrorMessage = function (title, message) {
				// Call the global function to show error message
				showErrorMessage(title, message);
			};

			return createSampleModalService;

		}]);

	createSampleModule.controller('createSampleDatasetOptionCtrl', ['$scope', '$uibModal', '$uibModalInstance', 'studyContext', 'createSampleModalService',
		function ($scope, $uibModal, $uibModalInstance, studyContext, createSampleModalService) {

			$scope.modalTitle = 'Create genotyping samples';
			$scope.message = 'Select the dataset from where you would like to generate genotyping samples';
			$scope.measurementDatasetId = studyContext.measurementDatasetId;
			$scope.selected = {datasetId: $scope.measurementDatasetId};

			$scope.next = function () {
				createSampleModalService.openSelectEnvironmentToSampleListModal($scope.selected.datasetId);
				$uibModalInstance.close();
			};

		}]);

	createSampleModule.controller('selectEnvironmentToSampleListModalCtrl', ['$scope', 'datasetService',
		'createSampleModalService', '$uibModalInstance', 'studyContext', 'datasetId',
		function ($scope, datasetService, createSampleModalService, $uibModalInstance, studyContext, datasetId) {

			$scope.instances = [];
			$scope.selectedInstances = {};
			$scope.isEmptySelection = false;

			$scope.continueCreatingSampleList = function () {

				var instanceNumbers = [];
				Object.keys($scope.selectedInstances).forEach(function (instanceNumber) {
					var isSelected = $scope.selectedInstances[instanceNumber];
					if (isSelected) {
						instanceNumbers.push(instanceNumber);
					}
				});

				if ($scope.isEmptySelection) {
					createSampleModalService.showErrorMessage('', $.fieldbookMessages.errorNotSelectedInstance);
				} else {
					createSampleModalService.openSelectSelectionVariableToSampleListModal(datasetId, instanceNumbers);
					$uibModalInstance.close();
				}
			};

			$scope.cancel = function () {
				$uibModalInstance.close();
			}

			$scope.init = function () {

				datasetService.getDatasetInstances(datasetId).then(function (instances) {
					$scope.instances = instances;
				});
			};

			$scope.init();

		}]);

	createSampleModule.controller('selectSelectionVariableToSampleListModalCtrl', ['$scope', 'datasetService', '$http',
		'$timeout', 'createSampleModalService', 'instanceNumbers', 'datasetId', '$uibModalInstance', 'VARIABLE_TYPES',
		function ($scope, datasetService, $http, $timeout, createSampleModalService, instanceNumbers, datasetId, $uibModalInstance, VARIABLE_TYPES) {

			$scope.backToCreateSample = function () {
				createSampleModalService.openSelectEnvironmentToSampleListModal(datasetId);
				$scope.cancel();
			};

			$scope.init = function () {

				$scope.variables = [];
				$scope.saveSampleListButton = false;
				$scope.dateSampling = '';
				$scope.users = [];
				$scope.selectedUser = undefined;
				$scope.variableSelected = undefined;
				$scope.listOwner = '';

				datasetService.getVariables(datasetId, VARIABLE_TYPES.SELECTION_METHOD).then(function (variables) {
					$scope.variables = variables;
					if ($scope.variables.length === 0) {
						createSampleModalService.showErrorMessage('', $.fieldbookMessages.errorNoVarietiesSamples);
					}
				});

				$http.get('/bmsapi/users/filter?cropName='+ cropName +'&programUUID=' + currentProgramId).success(function (data) {
					$scope.users = data;

					angular.forEach($scope.users, function (user) {
						if (user.id === loggedInUserId) {
							$scope.selectedUser = user.id;
							$scope.listOwner = user.firstName + " " + user.lastName;
						}
						$timeout(function () {
							angular.element('#sampleSelectUser').select2();

						}, 1);
					});
				}).error(function (data) {
					if (data.status === 401) {
						bmsAuth.handleReAuthentication();
					}
					createSampleModalService.showErrorMessage('', data.errors[0].message);
					$scope.selectedUser = [];
				});

				$timeout(function () {
					angular.element('#sampleSelectVariable').focus();
					angular.element('#sampleSelectVariable').select2();
					angular.element('#sampleSelectSamplingDate').datepicker({dateFormat: "yyyy-mm-dd"}).val('');
				}, 1);
			};

			$scope.cancel = function () {
				$uibModalInstance.close();
			};

			$scope.continue = function () {
				$scope.saveSampleListButton = true;
				$scope.sampleList = {
					"description": "",
					"notes": "",
					"createdBy": $scope.listOwner,
					"selectionVariableId": $scope.variableSelected.id,
					"instanceIds": instanceNumbers,
					"takenBy": "",
					"samplingDate": $scope.dateSampling,
					"datasetId": datasetId,
					"cropName": cropName,
					"programUUID": currentProgramId,
					"parentId": 0,
					"listName": "",
					"createdDate": ""
				};

				if ($scope.selectedUser !== null) {
					angular.forEach($scope.users, function (user) {
						if (user.id === $scope.selectedUser) {
							$scope.sampleList.takenBy = user.username;
						}
					});
				}
				$scope.saveSampleListButton = false;
				SaveSampleList.openSaveSampleListModal($scope.sampleList);
				$uibModalInstance.close();
			};

			$scope.init();
		}]);

})();
