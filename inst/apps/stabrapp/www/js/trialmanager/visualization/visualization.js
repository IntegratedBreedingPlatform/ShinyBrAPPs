(function () {
	'use strict';

	var visualizationModule = angular.module('visualization', ['r-package', 'datasets-api']);

	visualizationModule.factory('visualizationModalService', ['$uibModal', function ($uibModal) {

		var visualizationModalService = {};

		visualizationModalService.openModal = function (datasetId, observationUnitsSearch) {
			return $uibModal.open({
				templateUrl: '/Fieldbook/static/angular-templates/visualization/visualizationModal.html',
				controller: 'visualizationModalController',
				size: 'md',
				resolve: {
					datasetId: function () {
						return datasetId;
					},
					observationUnitsSearch: function () {
						return observationUnitsSearch;
					}
				}
			});
		};

		visualizationModalService.showImageModal = function (imageUrl) {
			return $uibModal.open({
				templateUrl: '/Fieldbook/static/angular-templates/visualization/showImageModal.html',
				controller: 'showImageModalController',
				size: 'lg',
				resolve: {
					imageUrl: function () {
						return imageUrl;
					}
				}
			});
		};

		return visualizationModalService;

	}]);

	visualizationModule.controller('visualizationModalController', ['$scope', '$q', '$timeout', '$uibModalInstance', 'rPackageService', 'datasetService',
		'datasetId', 'observationUnitsSearch', 'visualizationModalService',
		function ($scope, $q, $timeout, $uibModalInstance, rPackageService, datasetService, datasetId, observationUnitsSearch, visualizationModalService) {

			var OBSERVATION_UNIT_ID = 8201;
			var PLOT_TYPES = {
				SCATTERPLOT: 'Scatterplot',
				BOXPLOT: 'Boxplot',
				HISTOGRAM: 'Histogram'
			};

			$scope.rCalls = [];
			$scope.selection = {selectedRCall: null, selectedVariableX: null, selectedVariableY: null};
			$scope.variates = [];
			$scope.factors = [];
			$scope.regressionMethods = [{
				method: '\"auto\"',
				description: 'auto'
			}, {
				method: '\"lm\"',
				description: 'linear model'
			}, {
				method: '\"glm\"',
				description: 'generalized linear model'
			}, {
				method: '\"gam\"',
				description: 'generalized additive model'
			}, {
				method: '\"loess\"',
				description: 'loess regression'
			}];

			this.$onInit = function () {
				$scope.init();
			}

			$scope.$watch('selection.selectedRCall', function () {
				$timeout(function () {
					$scope.selection.selectedVariableX = null;
					$scope.selection.selectedVariableY = null;
				}, 0);
			});

			$scope.init = function () {
				var qplotPackageId = 3;
				rPackageService.getRCallsObjects(qplotPackageId).then(function (data) {
					$scope.rCalls = data;
					$scope.selection.selectedRCall = data[0];
				});
				datasetService.getColumns(datasetId, observationUnitsSearch.draftMode).then(function (columnsData) {

					var variateVariableTypes = ['TRAIT', 'SELECTION_METHOD'];

					$scope.variates = columnsData.filter(column => {
						return variateVariableTypes.includes(column.variableType);
					});
					$scope.factors = columnsData.filter(column => {
						return !variateVariableTypes.includes(column.variableType) && OBSERVATION_UNIT_ID !== column.termId;
					});
				});
			};

			$scope.cancel = function () {
				$uibModalInstance.close();
			};

			$scope.generate = function () {
				if (validate($scope.selection.selectedRCall, $scope.selection.selectedVariableX, $scope.selection.selectedVariableY)) {
					var rCall = prepareParameters(angular.copy($scope.selection.selectedRCall), $scope.selection.selectedVariableX, $scope.selection.selectedVariableY);
					observationUnitsSearch.filterColumns = getFilterColumns($scope.selection.selectedRCall, $scope.selection.selectedVariableX, $scope.selection.selectedVariableY);
					datasetService.getObservationForVisualization(datasetId, JSON.stringify(observationUnitsSearch)).then(function (data) {
						rCall.parameters.data = JSON.stringify(data);
						return rPackageService.executeRCall(rCall.endpoint, rCall.parameters);
					}).then(function (response) {
						visualizationModalService.showImageModal(response.headers().location + 'graphics/1/svg');
					});
				}
			};

			function validate(rCall, variableX, variableY) {
				if (!hasRequiredFields(rCall, variableX, variableY)) {
					showErrorMessage('', $.fieldbookMessages.errorPlotGraphRequiredFields);
					return false;
				}
				return true;
			}

			function hasRequiredFields(rCall, variableX, variableY) {
				if (PLOT_TYPES.SCATTERPLOT === rCall.description) {
					return variableX && variableY && rCall.parameters.method;
				} else if (PLOT_TYPES.HISTOGRAM === rCall.description) {
					return variableX;
				} else if (PLOT_TYPES.BOXPLOT === rCall.description) {
					return variableX && variableY;
				}
			}

			function getFilterColumns(rCall, selectedVariableX, selectedVariableY) {
				// Only the variables selected from the UI should be included in the data that will be sent to OpenCPU.
				var filterColumns = [];
				filterColumns.push(selectedVariableX.name);
				if ('Histogram' !== rCall.description) {
					filterColumns.push(selectedVariableY.name);
				}
				return filterColumns;
			}

			function prepareParameters(rCall, selectedVariableX, selectedVariableY) {
				// Make sure that field names are wrapped in backticks (`) to escape the spaces in them if there's any.
				// If plot graph is a Boxplot, we must wrap the X field name with R 'factor' function
				// so that R will know that the field is a factor
				rCall.parameters.x = PLOT_TYPES.BOXPLOT === rCall.description ? 'factor(' + wrapTextWith('`', selectedVariableX.name) + ')' : wrapTextWith('`', selectedVariableX.name);
				// Set x and y labels with variable alias
				rCall.parameters.xlab = wrapTextWith('"', selectedVariableX.alias);
				if (PLOT_TYPES.HISTOGRAM !== rCall.description) {
					rCall.parameters.y = wrapTextWith('`', selectedVariableY.name);
					rCall.parameters.ylab = wrapTextWith('"', selectedVariableY.alias);
				}
				return rCall;
			}

			function wrapTextWith(character, text) {
				return character + text + character;
			}

		}]);


	visualizationModule.controller('showImageModalController', ['$scope', '$uibModalInstance', 'imageUrl', function ($scope, $uibModalInstance, imageUrl) {

		$scope.imageUrl = imageUrl;

		$scope.cancel = function () {
			$uibModalInstance.close();
		};

	}]);


	visualizationModule.directive('numbersOnly', function () {
		return {
			require: 'ngModel',
			link: function (scope, element, attr, ngModelCtrl) {
				function fromUser(text) {
					if (text) {
						var transformedInput = text.replace(/[^0-9.]/g, '');

						if (transformedInput !== text) {
							ngModelCtrl.$setViewValue(transformedInput);
							ngModelCtrl.$render();
						}
						return transformedInput;
					}
					return undefined;
				}
				ngModelCtrl.$parsers.push(fromUser);
			}
		};
	});

})();