/*globals angular, isStudyNameUnique, showSuccessfulMessage, isCategoricalDisplay,
 showInvalidInputMessage, studyFieldsIsRequired,saveSuccessMessage,validateStartEndDateBasic, showAlertMessage, doSaveImportedData,
 invalidTreatmentFactorPair,unpairedTreatmentFactor,createErrorNotification,openStudyTree,validateAllDates, showErrorMessage*/
(function() {
	'use strict';
	angular.module('manageTrialApp').service('TrialManagerDataService', ['GERMPLASM_LIST_SIZE','GERMPLASM_CHECKS_SIZE', 'TRIAL_SETTINGS_INITIAL_DATA',
		'SAMPLE_LIST_DATA','ENVIRONMENTS_INITIAL_DATA', 'GERMPLASM_INITIAL_DATA', 'EXPERIMENTAL_DESIGN_INITIAL_DATA',
		'EXPERIMENTAL_DESIGN_SPECIAL_DATA', 'TREATMENT_FACTORS_INITIAL_DATA', 'BASIC_DETAILS_DATA', '$http', '$resource', 'TRIAL_MANAGEMENT_MODE', 'UNSPECIFIED_LOCATION_ID', 'BREEDING_METHOD_CODE', '$q',
		'TrialSettingsManager','studyStateService', '_', '$localStorage','$rootScope', 'studyContext', 'derivedVariableService', 'experimentDesignService',
		function(GERMPLASM_LIST_SIZE, GERMPLASM_CHECKS_SIZE, TRIAL_SETTINGS_INITIAL_DATA, SAMPLE_LIST_DATA, ENVIRONMENTS_INITIAL_DATA, GERMPLASM_INITIAL_DATA,
					EXPERIMENTAL_DESIGN_INITIAL_DATA, EXPERIMENTAL_DESIGN_SPECIAL_DATA, TREATMENT_FACTORS_INITIAL_DATA, BASIC_DETAILS_DATA, $http, $resource,
					TRIAL_MANAGEMENT_MODE, UNSPECIFIED_LOCATION_ID, BREEDING_METHOD_CODE, $q, TrialSettingsManager, studyStateService, _, $localStorage, $rootScope, studyContext, derivedVariableService, experimentDesignService) {


			// TODO: clean up data service, at the very least arrange the functions in alphabetical order
			var extractData = function(initialData, initializeProperty) {
				if (!initialData) {
					var data = {};
					if (initializeProperty) {
						data[initializeProperty] = {};
					}

					return data;
				} else {
					return initialData.data;
				}
			};

			var dataRegistry = {};
			var settingRegistry = {};
			var settingsArray = [];
			var saveEventListeners = {};
            var TRIAL_LOCATION_NAME_INDEX = 8180;
			var LOCATION_NAME_ID = 8190;
			var TRIAL_INSTANCE_INDEX = 8170;
			var selectedEnviromentOnMeasurementTab = {};
			var propagateChange = function(targetRegistry, dataKey, newValue) {
				if (targetRegistry[dataKey]) {
					angular.forEach(targetRegistry[dataKey], function(updateFunction) {
						updateFunction(newValue);
					});
				}
			};

			var extractBasicDetailsData = function(initialData, initializeProperty) {
				var data = extractData(initialData, initializeProperty);

				// data saved from the database is htmlEscaped so we need to un-escape the retrieved data first.
				_.each(data.basicDetails, function(val, key) {
					data.basicDetails[key] = _.unescape(val);
				});

				if (data.startDate === null || data.startDate === '') {
					data.startDate = $.datepicker.formatDate('yy-mm-dd', new Date());
				}

				return data;
			};

			var updateFrontEndTrialData = function(trialID, updateFunction) {
				$http.get('/Fieldbook/TrialManager/openTrial/updateSavedTrial?trialID=' + trialID).success(function(data) {
					if (updateFunction) {
						updateFunction(data);
					} else {
						service.currentData.basicDetails.studyID = trialID;
					}
				}).error(function() {
					showErrorMessage('', $.fieldbookMessages.errorSaveStudy);
				});
			};

			var extractSettings = function(initialData) {

				if (initialData) {
					if (!initialData.settingMap) {
						var data = new angular.OrderedHash();
						data.addList(initialData.settings, function(item) {
							item.extracted = true;
							return item.variable.cvTermId;
						});

						return data;
					} else {
						var dataMap = {};

						$.each(initialData.settingMap, function(key, value) {
							dataMap[key] = new angular.OrderedHash();
							dataMap[key].addList(value, function(item) {
								item.extracted = true;
								return item.variable.cvTermId;
							});
						});

						return dataMap;
					}

				}

				return new angular.OrderedHash();
			};

			var extractTreatmentFactorSettings = function(initialData) {
				var settingMap = {};
				if (initialData) {
					if (initialData.settingMap && initialData.settingMap.details) {
						var data = new angular.OrderedHash();
						data.addList(initialData.settingMap.details, function(item) {
							item.extracted = true;
							return item.variable.cvTermId;
						});

						settingMap.details = data;
					}

					if (initialData.settingMap && initialData.settingMap.treatmentLevelPairs) {
						settingMap.treatmentLevelPairs = initialData.settingMap.treatmentLevelPairs;
						angular.forEach(settingMap.treatmentLevelPairs, function(value, key) {
							var data = new angular.OrderedHash();
							data.addList(value, function(item) {
								item.extracted = true;
								return item.variable.cvTermId;
							});

							settingMap.treatmentLevelPairs[key] = data;
						});
					}
				} else {
					settingMap.details = new angular.OrderedHash();
					settingMap.treatmentLevelPairs = {};
				}

				return settingMap;
			};

			// TODO: change function such that it does not require jQuery style element / id based access for value retrieval
			var submitGermplasmList = function() {
				var $form = $('#germplasm-list-form');
				$('#startIndex').val($('#startIndex2').val());
				$('#interval').val($('#interval2').val());
				$('#mannerOfInsertion').val($('#mannerOfInsertion2').val());
				var columnsOrder = ($('#measurement-table') && $('#measurement-table').length !== 0 && service.isOpenStudy()) ?
					BMS.Fieldbook.MeasurementsTable.getColumnOrdering('measurement-table') : [];

				var serializedData = $form.serializeArray();
				serializedData[serializedData.length] = {name: 'columnOrders', value: (JSON.stringify(columnsOrder))};
				var d = $q.defer();

				$http.post('/Fieldbook/TrialManager/GermplasmList/next', $.param(serializedData),
					{headers: {'Content-Type': 'application/x-www-form-urlencoded'}}).success(function(data) {
					d.resolve(data);
				}).error(function(data, status, header, config) {
                      showErrorMessage('', 'Could not persist the germplasm list.');
                });

				return d.promise;
			};

			var recreateSessionVariablesTrial = function() {
				$.ajax({
					url: '/Fieldbook/TrialManager/openTrial/recreate/session/variables',
					type: 'GET',
					data: '',
					cache: false,
					success: function(html) {
						showSuccessfulMessage('', saveSuccessMessage);
					}
				});
			};

			var notifySaveEventListeners = function() {
				angular.forEach(saveEventListeners, function(saveListenerFunction) {
					saveListenerFunction();
				});
			};

			var cleanupData = function(values) {
				if (values) {
					angular.forEach(values, function(value, key) {
						if(key === BREEDING_METHOD_CODE){
							if (value && (value.key || value.key === 0)) {
								values[key] = value.key;
							}
						}else{
							if (value && (value.id || value.id === 0)) {
								values[key] = value.id;
							}
						}

					});
				}
			};

			var VariablePairService = $resource('/Fieldbook/TrialManager/createTrial/retrieveVariablePairs/:id',
				{id: '@id'}, {get: {method: 'get', isArray: true}});

			var service = {
				// user input data and default values of standard variables
				currentData: {
					trialSettings: extractData(TRIAL_SETTINGS_INITIAL_DATA),
					instanceInfo: extractData(ENVIRONMENTS_INITIAL_DATA),
					basicDetails: extractBasicDetailsData(BASIC_DETAILS_DATA),
					treatmentFactors: extractData(TREATMENT_FACTORS_INITIAL_DATA, 'currentData'),
					experimentalDesign: extractData(EXPERIMENTAL_DESIGN_INITIAL_DATA)
				},
				// standard variable [meta-data] information or a particular tab settings information
				// what I get is an instance of OrderedHash containing an array of keys with the map
				settings: {
					trialSettings: extractSettings(TRIAL_SETTINGS_INITIAL_DATA),
					environments: extractSettings(ENVIRONMENTS_INITIAL_DATA),
					germplasm: extractSettings(GERMPLASM_INITIAL_DATA),
					treatmentFactors: extractTreatmentFactorSettings(TREATMENT_FACTORS_INITIAL_DATA),
					basicDetails: extractSettings(BASIC_DETAILS_DATA),
					sampleList: SAMPLE_LIST_DATA
				},
				applicationData: {
					unsavedTreatmentFactorsAvailable: false,
					germplasmListCleared: false,
					isGeneratedOwnDesign: false,
					advanceType: 'study',
					hasNewInstanceAdded: false,
					germplasmListSelected: GERMPLASM_LIST_SIZE > 0,
					germplasmChangesUnsaved: false,
					designTypes: [],
					deleteEnvironmentCallback: function() {}
				},

				specialSettings: {
					// settings that has special data structure
					experimentalDesign: {
						factors: (function() {
							var hardFactors = new angular.OrderedHash();
							if (EXPERIMENTAL_DESIGN_SPECIAL_DATA && EXPERIMENTAL_DESIGN_SPECIAL_DATA.data) {
								hardFactors.addList(EXPERIMENTAL_DESIGN_SPECIAL_DATA.data.expDesignDetailList, function(item) {
									return item.variable.cvTermId;
								});
							}

							return hardFactors;

						})(),
						germplasmTotalListCount: GERMPLASM_LIST_SIZE,
						germplasmTotalCheckCount: GERMPLASM_CHECKS_SIZE,

						showAdvancedOptions: [false, false, false]
					},
					treatmentLevelPairs: {}
				},

                performDataCleanup: function() {
                    // TODO: delegate the task of cleaning up data to each tab that produces it, probably via listener

                    // perform cleanup of data for study settings
                    // right now, just make sure that no objects are sent as user input for user-defined settings
                    cleanupData(service.currentData.trialSettings.userInput);
                    angular.forEach(service.currentData.treatmentFactors.currentData, function(treatmentFactor) {
                        cleanupData(treatmentFactor.labels);
                    });
                    angular.forEach(service.currentData.instanceInfo.instances, function(instance) {
                        cleanupData(instance.managementDetailValues);
                        cleanupData(instance.trialDetailValues);
                    });
                },

				// returns a promise object to be resolved later
				retrieveVariablePairs: function(cvTermId) {
					return VariablePairService.get({id: cvTermId}).$promise;
				},

				updateSelectedFolder: function(folderID) {
					service.currentData.basicDetails.folderId = folderID;
				},
				// the data param structures
				generateExpDesign: function(data) {
					var GenerateExpDesignService = $resource('/Fieldbook/TrialManager/experimental/design/generate', {}, {});
					return GenerateExpDesignService.save(data).$promise;
				},

				deleteGenerateExpDesign: function(measurementDatasetId) {
					var GenerateExpDesignService = $resource('/Fieldbook/TrialManager/experimental/design/delete/:measurementDatasetId',{datasetId: '@measurementDatasetId'},{delete: {method:'delete'}});
					return GenerateExpDesignService.delete({measurementDatasetId: measurementDatasetId}).$promise;
				},

				retrieveDesignType: function() {
					experimentDesignService.getDesignTypes().then(function(designTypes) {
						service.applicationData.designTypes = designTypes;
					});
				},

				retrieveInsertionManner: function () {
					experimentDesignService.getInsertionManners().then(function(insertionManners) {
						service.applicationData.insertionManners = insertionManners;
					});
				},

				getDesignTypeById : function(designTypeId, designTypes) {
					return _.find(designTypes, function (designType) { return designType.id === Number(designTypeId) });
				},

				retrieveGenerateDesignInput: function(designType) {
					var environmentData = angular.copy(service.currentData.instanceInfo);

					_.each(environmentData.environments, function(data, key) {
						_.each(data.managementDetailValues, function(value, key) {
							if (value && value.id) {
								data.managementDetailValues[key] = value.id;
							}
						});
					});

					var data = {
						environmentData: environmentData,
						selectedExperimentDesignType: angular.copy(service.getDesignTypeById(designType, service.applicationData.designTypes)),
						startingPlotNo: service.currentData.experimentalDesign.startingPlotNo,
						hasNewInstanceAdded: service.applicationData.hasNewInstanceAdded
					};

					return data;
				},

				isOpenStudy: function() {
					return service.currentData.basicDetails.studyID !== null &&
						service.currentData.basicDetails.studyID !== 0;
				},

				isLockedStudy: function() {
					return service.currentData.basicDetails.isLocked;
				},

				changeLockedStatus : function(doLock) {

					var studyId = service.currentData.basicDetails.studyID;
					var study = {
						"locked": doLock
					};
					$http
						.patch('/bmsapi/crops/' + studyContext.cropName + '/programs/'+ studyContext.programId + '/studies/' + studyId, study)
						.success(function(data) {
							if (doLock) {
								showSuccessfulMessage('', lockStudySuccessMessage);
							} else {
								showSuccessfulMessage('', unlockStudySuccessMessage);
							}
							service.currentData.basicDetails.isLocked = doLock;
						});

				},

				deleteInstance: function(index) {
					var refreshMeasurementDeferred = $q.defer();
					var deleteMeasurementPossible = index !== 0;
					return refreshMeasurementDeferred.promise;
				},

				indicateUnsavedTreatmentFactorsAvailable: function() {
					if (!service.applicationData.unsavedTreatmentFactorsAvailable) {
						service.applicationData.unsavedTreatmentFactorsAvailable = true;
						if (service.currentData.experimentalDesign.designType === 3) {
							service.currentData.experimentalDesign.designType = null;
						}
					}
				},

				clearUnappliedChangesFlag: function() {
					service.applicationData.unsavedTreatmentFactorsAvailable = false;
				},
				extractData: extractData,
				extractSettings: extractSettings,
				extractTreatmentFactorSettings: extractTreatmentFactorSettings,
				saveCurrentData: function() {

					var missingLocations = service.currentData.instanceInfo.instances.some(function (instance) {
						return !instance.managementDetailValues ||
							(!instance.managementDetailValues[8190] && instance.managementDetailValues[8190] !== 0);
					});

					if (missingLocations) {
						showErrorMessage('', "There are some environments that don't have any location selected");
						return false;
					}

					if (service.applicationData.unsavedTreatmentFactorsAvailable && studyStateService.hasUnsavedData()) {
						showErrorMessage('', unsavedTreatmentFactor);
					} else if (service.applicationData.unsavedTreatmentFactorsAvailable) {
							showErrorMessage('', 'TREATMENT FACTORS will be saved automatically when generating the design.');
					} else if (service.isCurrentTrialDataValid(service.isOpenStudy())) {
						service.performDataCleanup();
						var columnsOrder =  ($('#measurement-table') && $('#measurement-table').length !== 0 && service.isOpenStudy()) ?
							BMS.Fieldbook.MeasurementsTable.getColumnOrdering('measurement-table') : [];
						var serializedData = (JSON.stringify(columnsOrder));
						if (!service.isOpenStudy()) {
							service.currentData.columnOrders = serializedData;
							$http.post('/Fieldbook/TrialManager/createTrial', service.currentData).then(function () {
								submitGermplasmList().then(function (generatedID) {
									showSuccessfulMessage('', saveSuccessMessage);
									notifySaveEventListeners();
									window.location = '/Fieldbook/TrialManager/openTrial/' + generatedID;
									studyStateService.resetState();
								});
							}, function (response) {
								if (response.data && response.data.errors) {
									showErrorMessage('',  response.data.errors[0].message);
								} else {
									showErrorMessage('', $.fieldbookMessages.errorSaveStudy);
								}
							});
						} else {
							if (studyStateService.hasGeneratedDesign()) {
								service.currentData.columnOrders = serializedData;
								$http.post('/Fieldbook/TrialManager/openTrial?replace=0', service.currentData).success(function(data) {
									recreateSessionVariablesTrial();
									notifySaveEventListeners();
									updateFrontEndTrialData(service.currentData.basicDetails.studyID, function (updatedData) {
										service.updateCurrentData('trialSettings', extractData(updatedData.trialSettingsData));
										service.updateSettings('trialSettings', extractSettings(updatedData.trialSettingsData));
										setupSettingsVariables();
										studyStateService.resetState();
									});
								}).error(function (response) {
									if (response.data && response.data.errors) {
										showErrorMessage('', response.data.errors[0].message);
									} else {
										showErrorMessage('', $.fieldbookMessages.errorSaveStudy);
									}
								});
							} else {
								service.currentData.columnOrders = serializedData;
								$http.post('/Fieldbook/TrialManager/openTrial?replace=1', service.currentData).then(function () {
									submitGermplasmList().then(function (trialID) {
										showSuccessfulMessage('', saveSuccessMessage);
										notifySaveEventListeners();
										window.location = '/Fieldbook/TrialManager/openTrial/' + trialID;

										derivedVariableService.displayExecuteCalculateVariableMenu();
									}, function () {
										showErrorMessage('', $.fieldbookMessages.errorSaveStudy);
									});
								}, function (response) {
									if (response.data && response.data.errors) {
										showErrorMessage('', response.data.errors[0].message);
									} else {
										showErrorMessage('', $.fieldbookMessages.errorSaveStudy);
									}
								});
							}

						}
					}
					service.resetServiceBackup();
				},
				onUpdateData: function(dataKey, updateFunction) {
					if (!dataRegistry[dataKey]) {
						dataRegistry[dataKey] = [];
						dataRegistry[dataKey].push(updateFunction);
					} else if (dataRegistry[dataKey].indexOf(updateFunction) === -1) {
						dataRegistry[dataKey].push(updateFunction);
					}
				},

				updateCurrentData: function(dataKey, newValue) {
					_.each(_.keys(newValue), function(nvkey) {
						service.currentData[dataKey][nvkey] = newValue[nvkey];
					});
					propagateChange(dataRegistry, dataKey, newValue);
				},

				updateSettings: function(key, newValue) {

					if (service.settings[key] instanceof angular.OrderedHash) {
						service.settings[key].removeAll();

						_.each(newValue.keys(), function(nvkey) {
							service.settings[key].push(nvkey, newValue.val(nvkey));
						});
					} else {
						_.each(_.keys(newValue), function(nvkey) {

							if (newValue[nvkey] instanceof angular.OrderedHash) {

								service.settings[key][nvkey].removeAll();

								_.each(newValue[nvkey].keys(), function(ohkey) {
									service.settings[key][nvkey].push(ohkey, newValue[nvkey].val(ohkey));
								});
							} else {
								service.settings[key][nvkey] = newValue[nvkey];
							}

						});
					}

					propagateChange(settingRegistry, key, newValue);
					settingsArray = [];
				},

				onUpdateSettings: function(key, updateFunction) {
					if (!settingRegistry[key]) {
						settingRegistry[key] = [];
						settingRegistry[key].push(updateFunction);
					} else if (settingRegistry[key].indexOf(updateFunction) === -1) {
						settingRegistry[key].push(updateFunction);
					}
				},

				registerSaveListener: function(name, saveListenerFunction) {
					saveEventListeners[name] = saveListenerFunction;
				},

				treatmentFactorDataInvalid: function() {
					var errorCode = 0;

					angular.forEach(service.currentData.treatmentFactors.currentData, function(value) {
						// check if a factor selected as a pair is also used as
						if (!value.variableId || value.variableId === 0) {
							errorCode = 2;
							return false;
						}

						if (service.currentData.treatmentFactors.currentData[value.variableId]) {
							errorCode = 1;
							return false;
						}
					});

					return errorCode;
				},

				constructDataStructureFromDetails: function(details) {
					var returnVal = {};
					$.each(details.vals(), function(key, value) {
						returnVal[value.variable.cvTermId] = null;
					});

					return returnVal;
				},

				transformViewSettingsVariable: function(settingVar) {
					settingVar.isChecked = false;
					settingVar.existingData = false;
					return settingVar;
				},

				/* this function returns a promise with the checkedCvtermIds map as the result */
				removeSettings: function(mode, _settings) {
					var deferred = $q.defer();
					// This will retrieve only the .isChecked of the variable-settings by filtering out the settings collection.
					var checkedCvtermIds = _.pairs(_settings.vals())
						.filter(function(val) {
							return _.last(val).isChecked;
						})
						.map(function(val) {
							return parseInt(_.first(val));
						});

					if (checkedCvtermIds.length > 0) {
						$http.post('/Fieldbook/manageSettings/deleteVariable/' + mode, checkedCvtermIds)
							.success(function() {
								_(checkedCvtermIds).each(function(varIds) {
									_settings.remove(varIds);
								});

								deferred.resolve(checkedCvtermIds);
							});
					} else {
						deferred.resolve([]);
					}

					return deferred.promise;
				},

				getSettingsArray: function() {
					if (settingsArray.length === 0) {
						angular.forEach(service.settings, function(value, key) {
							if (key === 'environments') {
								settingsArray.push(value.managementDetails);
								settingsArray.push(value.trialConditionDetails);
							} else if (key === 'experimentalDesign') {
								return true;
							} else if (key === 'treatmentFactors') {
								settingsArray.push(value.details);
							} else if (key === 'sampleList') {
								return true;
							} else {
								if (value) {
									settingsArray.push(value);
								}

							}
						});
					}

					return settingsArray;
				},

				isCurrentTrialDataValid: function(isEdit) {
					if (isEdit === undefined) {
						isEdit = false;
					}

					var hasError = false, name = '', customMessage = '', errorCode = 0;
					var creationDate = service.currentData.basicDetails.startDate;
					var completionDate = service.currentData.basicDetails.endDate;
					if (!service.currentData.basicDetails.folderId || service.currentData.basicDetails.folderId === '') {
						hasError = true;
						name = $('#folderLabel').text();
						openStudyTree(2, service.updateSelectedFolder, true);
						return false;
					} else if ($.trim(service.currentData.basicDetails.studyName) === '') {
						hasError = true;
						name = 'Name';
					} else if ($.trim(service.currentData.basicDetails.description) === '') {
						hasError = true;
						name = 'Description';
					} else if (!isEdit && isStudyNameUnique(service.currentData.basicDetails.studyName) === false) {
						hasError = true;
						customMessage = 'Name should be unique';
					} else if (creationDate === '') {
						// validate creation date
						hasError = true;
						name = 'Creation Date';
					} else if (service.currentData.instanceInfo.numberOfInstances <= 0) {
						hasError = true;
						customMessage = 'the study should have at least one environment';
					} else if ($.trim(service.currentData.basicDetails.studyType) === '') {
						hasError = true;
						name = 'Study type';
					} else if (!service.currentData.basicDetails.folderId || service.currentData.basicDetails.folderId === '') {
						hasError = true;
						name = $('#folderLabel').text();
						openStudyTree(2, service.updateSelectedFolder, true);
						return false;
					}

					var invalidDateMsg = validateAllDates();
					if (invalidDateMsg !== '') {
						hasError = true;
						customMessage = invalidDateMsg;
					}
					if (!hasError) {
						errorCode = service.treatmentFactorDataInvalid();
						if (errorCode === 1) {
							hasError = true;
							customMessage = invalidTreatmentFactorPair;
						} else if (errorCode === 2) {
							hasError = true;
							customMessage = unpairedTreatmentFactor;
						}
					}

					if (hasError) {
						var errMsg = '';
						if (name !== '') {
							errMsg = name.replace('*', '').replace(':', '') + ' ' + studyFieldsIsRequired;
						}

						if (customMessage !== '') {
							errMsg = customMessage;
						}

						showInvalidInputMessage(errMsg);
						return false;
					}

					var valid = validateStartEndDateBasic(creationDate, completionDate);

					if (valid !== true) {
						showInvalidInputMessage(valid);
						return false;
					}

					// Validate all variables
					var isValidVariables = service.validateAllVariablesInput();

					if (isValidVariables && isValidVariables.hasError) {
						valid = valid && !isValidVariables.hasError;
						createErrorNotification(isValidVariables.customHeader, isValidVariables.customMessage);
					}

					//valid = false    // remove later

					return valid;

				},

				validateAllVariablesInput: function() {
					var results = {
						hasError: false,
						customMessage: '',
						customHeader: 'Invalid Input '
					};

					// perform validation on all settings.currentData, (min / max) if any
					// Validate all Trial Settings
					_.each(service.settings.trialSettings.vals(), function(item, key) {
						if (!(!item.variable.maxRange && !item.variable.minRange)) {
							_.find(service.currentData.trialSettings, function(val) {
								if (!!val[key]) {
									if (item.variable.maxRange < Number(val[key])) {
										results.customMessage = 'Invalid maximum range on variable ' + item.variable.name;
										results.hasError = true;
										return results.hasError;
									} else if (item.variable.minRange > Number(val[key])) {
										results.customMessage = 'Invalid minimum range on variable ' + item.variable.name;
										results.hasError = true;
										return results.hasError;
									}
								}
							});
						}
					});

					if (results.hasError) {
						return (function(tab) {
							results.customHeader += tab;
							return results;
						}('on Trial Settings'));
					}

					// validate environments
					_.each(service.settings.environments.managementDetails.vals(), function(item, key) {
						if (!(!item.variable.maxRange && !item.variable.minRange)) {
							// let's validate each environment
							_.find(service.currentData.instanceInfo.instances, function(val, index) {
								if (!!val.managementDetailValues[key]) {
									if (item.variable.maxRange < Number(val.managementDetailValues[key])) {
										results.customMessage = 'Invalid maximum range on management detail variable ' +
											item.variable.name + ' at environment ' + (Number(index) + Number(1));
										results.hasError = true;
										return results.hasError;
									} else if (item.variable.minRange > Number(val.managementDetailValues[key])) {
										results.customMessage = 'Invalid minimum range on management detail variable ' +
											item.variable.name + ' at environment ' + (Number(index) + Number(1));
										results.hasError = true;
										return results.hasError;
									}
								}
							});
						}
					});

					if (results.hasError) {
						return (function(tab) {
							results.customHeader += tab;
							return results;
						}('on Environments'));
					}

					_.each(service.settings.environments.trialConditionDetails.vals(), function(item, key) {
						if (!(!item.variable.maxRange && !item.variable.minRange)) {
							// letz validate each environment
							_.find(service.currentData.instanceInfo.instances, function(val, index) {
								if (!!val.trialDetailValues[key]) {
									if (item.variable.maxRange < Number(val.trialDetailValues[key])) {
										results.customMessage = 'Invalid maximum range on study details variable ' +
											item.variable.name + ' at environment ' + (Number(index) + Number(1));
										results.hasError = true;
										return results.hasError;
									} else if (item.variable.minRange > Number(val.trialDetailValues[key])) {
										results.customMessage = 'Invalid minimum range on study details variable ' +
											item.variable.name + ' at environment ' + (Number(index) + Number(1));
										results.hasError = true;
										return results.hasError;
									}
								}
							});
						}
					});

					if (results.hasError) {
						return (function(tab) {
							results.customHeader += tab;
							return results;
						}('on Environments'));
					}

					//  validate all treatments variable inputs
					return service.validateAllTreatmentFactorLabels(results);
				},

				validateAllTreatmentFactorLabels: function(results) {
					//  validate all treatments variable inputs
					_.find(service.currentData.treatmentFactors.currentData, function(item, key) {
						if (!service.currentData.treatmentFactors.currentData[key].variableId) {
							results.customHeader = 'Invalid Input ';
							results.customMessage = 'Please choose treatment factor label';
							results.hasError = true;
							return results.hasError;
						}

						var settingsVar = service.settings.treatmentFactors.treatmentLevelPairs[key].
							val(service.currentData.treatmentFactors.currentData[key].variableId).variable;

						if (item.levels <= parseInt('0')) {
							results.customHeader = 'Invalid Input ';
							results.customMessage = 'The number of Treatments must be greater than 0';
							results.hasError = true;
							return results.hasError;
						}

						if (!(!settingsVar.maxRange && !settingsVar.minRange)) {
							_.find(item.labels, function(val, index) {

								if (!!val) {
									if (settingsVar.maxRange < Number(val)) {
										results.customMessage = 'Invalid maximum range on variable ' +
											settingsVar.name + ' at level ' + (Number(index) + Number(1));
										results.hasError = true;
										return results.hasError;
									} else if (settingsVar.minRange > Number(val)) {
										results.customMessage = 'Invalid minimum range on variable ' +
											settingsVar.name + ' at level ' + (Number(index) + Number(1));
										results.hasError = true;
										return results.hasError;
									}
								}

							});
						}
					});

					if (results.hasError) {
						return (function(tab) {
							results.customHeader += tab;
							return results;
						}('on Treatment Factors'));
					}
				},
				getPreferredEnvironmentName: function (environment, preferredLocationVariable, managementDetails) {
					var preferredLocation = '';
					if (managementDetails.vals()[LOCATION_NAME_ID] !== undefined) {

						//create a map for location dropdown values
						var locationMap = {};

						angular.forEach(managementDetails.vals()[LOCATION_NAME_ID].allValues, function (locationVariable) {
							locationMap[locationVariable.id] = locationVariable;
						});

						var locationId = 0;
						if (environment.managementDetailValues[LOCATION_NAME_ID] !== undefined) {
							locationId = isNaN(environment.managementDetailValues[LOCATION_NAME_ID]) ?
								environment.managementDetailValues[LOCATION_NAME_ID].id :
								environment.managementDetailValues[LOCATION_NAME_ID];
						}

						if (locationId !== 0) {
							preferredLocation = locationMap[locationId].name;
						}

					}

					var preferredLocationVariableName = preferredLocationVariable === LOCATION_NAME_ID ? preferredLocation
						: environment.managementDetailValues[preferredLocationVariable];

					return preferredLocationVariableName;

				},
				// store the initial values on some service properties so that we can revert to it later
				storeInitialValuesInServiceBackup: function() {
					$localStorage.serviceBackup = {
						settings: angular.copy(service.settings),
						currentData: angular.copy(service.currentData),
						specialSettings: angular.copy(service.specialSettings),
						applicationData: angular.copy(service.applicationData)
					};
				},
				resetServiceBackup: function () {
					$localStorage.serviceBackup = null;
				}


			};

			service.retrieveDesignType();
			service.retrieveInsertionManner();

			// 5 is the group no of treatment factors
			TrialSettingsManager.addDynamicFilterObj(service.currentData.treatmentFactors, 5);

			// setup view-data specific properties to setting variables
			var setupSettingsVariables = function() {
				_.each(service.settings, function(val) {
					if (val instanceof angular.OrderedHash) {
						_.find(val.vals(), function(_val) {
							_val.existingData = true;
							_val.isChecked = false;
						});

					} else {
						_.each(val, function(_val) {

							if (_val instanceof angular.OrderedHash) {
								_.find(_val.vals(), function(__val) {
									__val.existingData = true;
									__val.isChecked = false;
								});
							}
						});
					}

				});
			};

			setupSettingsVariables();

			return service;
		}
	])

		.service('TrialSettingsManager', ['TRIAL_VARIABLE_SELECTION_LABELS', function(TRIAL_VARIABLE_SELECTION_LABELS) {
			var TrialSettingsManager = window.TrialSettingsManager;
			var settingsManager = new TrialSettingsManager(TRIAL_VARIABLE_SELECTION_LABELS);

			return {
				openVariableSelectionDialog: function(params) {
					settingsManager._openVariableSelectionDialog(params);
				},

				// @param = this map contains variables of the pair that will be filtered
				addDynamicFilterObj: function(_map, group) {
					settingsManager._addDynamicFilter(_map, group);
				}

			};
		}]);
})();
