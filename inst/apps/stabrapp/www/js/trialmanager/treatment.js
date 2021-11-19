/**
 * Created by cyrus on 7/1/14.
 */

/*global angular*/
/*global showBaselineTraitDetailsModal*/
(function() {
	'use strict';

	var manageTrialApp = angular.module('manageTrialApp');

	manageTrialApp.controller('TreatmentCtrl', ['$scope', 'TrialManagerDataService', '_', '$q', '$http', 'studyStateService',
		function($scope, TrialManagerDataService, _, $q, $http, studyStateService) {

			$scope.disableTreatment = studyStateService.hasGeneratedDesign();

			$scope.settings = TrialManagerDataService.settings.treatmentFactors;
			$scope.data = TrialManagerDataService.currentData.treatmentFactors;

			// watch $scope.settings, since we are sure that $scope.settings is an orderedhash even empty, we could just
			// use $watchCollection, for every added change we retrieve the 'AMOUNT' pairs dynamically. also create a
			// store to $scope.currentData for the variable levels.

			$scope.trialMeasurement = {hasMeasurement: studyStateService.hasGeneratedDesign()};
			TrialManagerDataService.onUpdateSettings('treatmentFactors', function(newValue) {
				TrialManagerDataService.specialSettings.treatmentLevelPairs = $scope.settings.treatmentLevelPairs;
			});

			// map containing the treatment factor level pairs
			TrialManagerDataService.specialSettings.treatmentLevelPairs = $scope.settings.treatmentLevelPairs;

			$scope.generateTreatmentLevelPair = function(key) {
				var deferred = $q.defer();
				TrialManagerDataService.retrieveVariablePairs(key).then(function(data) {
					$scope.settings.treatmentLevelPairs[key] = new angular.OrderedHash();
					angular.forEach(data, function(val1) {
						$scope.settings.treatmentLevelPairs[key].push(val1.variable.cvTermId, val1);
					});

					deferred.resolve();
				});

				return deferred.promise;
			};

			$scope.onAddVariable = function(result) {

				angular.forEach(result, function(val, key) {
					// there's no existing treatmentLevelPair
					if (!$scope.settings.treatmentLevelPairs[key]) {
						$scope.generateTreatmentLevelPair(key).then(function() {
							$scope.data.currentData[key] = {
								levels: 0,
								labels: [],
								variableId: 0
							};
						});
					} else {
						$scope.data.currentData[key] = {
							levels: 0,
							labels: [],
							variableId: 0
						};
					}
				});

				TrialManagerDataService.indicateUnsavedTreatmentFactorsAvailable();
			};

			$scope.invalidBlockSizeMsg = '<b class="text-danger">Invalid Block Size</b>';
			$scope.addVariable = function() {
				return !studyStateService.hasGeneratedDesign();
			};

			$scope.generateDropdownList = function(key) {
				if (!$scope.settings.treatmentLevelPairs[key]) {
					return [];
				} else {
					var options = _.filter($scope.settings.treatmentLevelPairs[key].vals(), function(entry) {
						var found = false;
						angular.forEach($scope.data.currentData, function(value, key2) {
							if (key == key2) {
								return true;
							}

							if (!value) {
								return true;
							} else if (value.variableId == entry.variable.cvTermId) {
								found = true;
								return false;
							} else if (value.variableId.variable && value.variableId.variable.cvTermId == entry.variable.cvTermId) {
								found = true;
								return false;
							}
						});
						return !found;
					});

					return options;
				}
			};

			$scope.generateDropdownOption = function(key) {

				var options = {
					data: function() {
						return {
							results: $scope.generateDropdownList(key)
						};
					},

					formatSelection: function(value) {
						return value.variable.name;
					},

					formatResult: function(value) {
						return value.variable.name;
					},

					minimumResultsForSearch: -1,
					id: function(value) {
						return value.variable.cvTermId;
					},
					idAsValue: true
				};

				if ($scope.data.currentData[key] && $scope.data.currentData[key].variableId) {
					options.initSelection = function(element, callback) {
						angular.forEach($scope.generateDropdownList(key), function(value) {
							if (value.variable.cvTermId === $scope.data.currentData[key].variableId) {
								callback(value);
							}
						});
					};
				}

				return options;
			};

			// TODO : fix the select2 initial selection so that this is no longer necessary
			$scope.retrievePairDetail = function(key) {
				if ($scope.data.currentData[key]) {
					var current = $scope.data.currentData[key];
					var pairId;
					if (current.variableId) {
						if (current.variableId.variable) {
							pairId = current.variableId.variable.cvTermId;
							$scope.data.currentData[key].variableId = current.variableId.variable.cvTermId;
						} else if (! isNaN(current.variableId)) {
							pairId = current.variableId;
						}
					}

					if (pairId) {
						return $scope.settings.treatmentLevelPairs[key].val(pairId);
					}

				}
			};

			$scope.isDisableLevel = function(key) {
				return !($scope.data.currentData[key] && $scope.data.currentData[key].variableId);
			};

			$scope.performDelete = function(key) {
				var numericKey = parseInt(key, 10);
				$http.post('/Fieldbook/manageSettings/deleteTreatmentFactorVariable', {
					levelID: numericKey,
					valueID: $scope.data.currentData[key].variableId ? $scope.data.currentData[key].variableId : 0
				}).then(function() {
					$scope.settings.details.remove(key);
					delete $scope.data.currentData[key];
					if(!$scope.settings.details.m_keys.length){
						TrialManagerDataService.applicationData.unsavedTreatmentFactorsAvailable = false;
						if (TrialManagerDataService.currentData.experimentalDesign.designType === 3) {
							TrialManagerDataService.currentData.experimentalDesign.designType = null;
						}
					}else {
						TrialManagerDataService.indicateUnsavedTreatmentFactorsAvailable();
					}
				});
			};

			$scope.onLabelChange = function() {
				TrialManagerDataService.indicateUnsavedTreatmentFactorsAvailable();
			};

			$scope.onLevelChange = function(key, levels) {
				if (isNaN(levels)) {
					return;
				}

				levels = parseInt(levels);

				var diff = Math.abs($scope.data.currentData[key].labels.length - levels);

				// remove items if no of levels is less thant array
				if ($scope.data.currentData[key].labels.length > levels) {
					while ($scope.data.currentData[key].labels.length > levels) {
						$scope.data.currentData[key].labels.pop();
					}
				} else {
				// add items if no of levels is more thant array
					for (var j = 0; j < diff; j++) {
						$scope.data.currentData[key].labels.push(null);
					}
				}

				TrialManagerDataService.indicateUnsavedTreatmentFactorsAvailable();
			};

		}]);

	})();
