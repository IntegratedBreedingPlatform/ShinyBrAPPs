/* global angular, showErrorMessage, showAlertMessage, showSuccessfulMessage, expDesignMsgs */
(function() {
		'use strict';
		var manageTrialAppModule = angular.module('manageTrialApp');

		manageTrialAppModule.constant('EXP_DESIGN_MSGS', expDesignMsgs)
			.constant('EXPERIMENTAL_DESIGN_PARTIALS_LOC', '/Fieldbook/static/angular-templates/experimentalDesignPartials/')
			.controller('ExperimentalDesignCtrl', ['$scope', '$state', 'EXPERIMENTAL_DESIGN_PARTIALS_LOC','DESIGN_TYPE','SYSTEM_DEFINED_ENTRY_TYPE', 'TrialManagerDataService', '$http',
				'EXP_DESIGN_MSGS', '_', '$q', 'Messages', '$rootScope', 'studyStateService', 'studyContext', 'experimentDesignService', '$uibModal', 'studyInstanceService', 'studyEntryService',
				function($scope, $state, EXPERIMENTAL_DESIGN_PARTIALS_LOC, DESIGN_TYPE, SYSTEM_DEFINED_ENTRY_TYPE, TrialManagerDataService, $http, EXP_DESIGN_MSGS, _,
						 $q, Messages, $rootScope, studyStateService, studyContext, experimentDesignService, $uibModal, studyInstanceService, studyEntryService) {

					var ENTRY_TYPE_COLUMN_DATA_KEY = '8255-key';
					var MESSAGE_DIV_ID = 'page-message';

					$scope.$on('$viewContentLoaded', function(){
						// This is to automatically refresh the design details for augmented design
						// whenever the Experimental tab is viewed
						if ($scope.data.designType === DESIGN_TYPE.AUGMENTED_RANDOMIZED_BLOCK) {
							$scope.refreshDesignDetailsForAugmentedDesign();
						}
						if ($scope.data.designType === DESIGN_TYPE.ENTRY_LIST_ORDER ) {
							$scope.refreshDesignDetailsForELODesign();
						}
						if ($scope.data.designType === DESIGN_TYPE.P_REP ) {
							$scope.refreshDesignDetailsForPRepDesign();
						}
					});

					$scope.applicationData = TrialManagerDataService.applicationData;
					$scope.studyID = TrialManagerDataService.currentData.basicDetails.studyID;

					$scope.designTypes = TrialManagerDataService.applicationData.designTypes;
					$scope.insertionManners = TrialManagerDataService.applicationData.insertionManners;
					$scope.designTypeView = [];
					$scope.insertionMannerView = [];

					$scope.generateDesignView = function() {

						// Add Breeding View Engine Designs except for for Custom Import Design
						$.each($scope.designTypes, function(index, designType){
							if(designType.name !== 'Custom Import Design'){
								$scope.designTypeView.push(designType);
							}
						});
					};

					$scope.generateInsertionMannerView = function() {
						$.each($scope.insertionManners, function(index, insertionManner){
							$scope.insertionMannerView.push(insertionManner);
						});
					};

					$scope.generateDesignView();
					$scope.generateInsertionMannerView();

					$scope.$watch(function() {
						return $scope.data.designType;
					}, function(newValue) {
						// If Design Type is Preset Design
						$scope.currentDesignType = TrialManagerDataService.getDesignTypeById(newValue, $scope.designTypes);
					});

					// TODO : re run computeLocalData after loading of previous study as template
					$scope.computeLocalData = function() {
						$scope.data.designType = TrialManagerDataService.currentData.experimentalDesign.designType;

						$scope.settings = TrialManagerDataService.specialSettings.experimentalDesign;
						$scope.settings.treatmentFactors = TrialManagerDataService.settings.treatmentFactors.details;

						// user has a treatment factor, if previous exp design is not RCBD, then set selection to RCBD
						// may need to clear non RCBD input
						if (TrialManagerDataService.settings.treatmentFactors.details.keys().length > 0) {
							$scope.data.designType = TrialManagerDataService.getDesignTypeById(DESIGN_TYPE.RANDOMIZED_COMPLETE_BLOCK, $scope.designTypes).id;
						}

						if ($scope.data.designType != null && $scope.data.designType !== '') {
							$scope.currentDesignType = TrialManagerDataService.getDesignTypeById($scope.data.designType, $scope.designTypes);
							if($scope.currentDesignType) {
								if ($scope.currentDesignType.params !== null) {
									$scope.currentParams = EXPERIMENTAL_DESIGN_PARTIALS_LOC + $scope.currentDesignType.params;
								} else {
									$scope.currentParams = null;
								}

								if (!$scope.settings.showAdvancedOptions[$scope.data.designType]) {
									$scope.settings.showAdvancedOptions[$scope.data.designType] = $scope.data.useLatenized;
								}

								if ($scope.currentDesignType !== null && $scope.currentDesignType.name === 'Custom Import Design') {
									$http.get('/Fieldbook/DesignImport/getCustomImportDesignTypeDetails').then(function(result) {
										$scope.currentDesignType.templateName = result.data.templateName;
									});
								}
							} else {
								$http.get('/Fieldbook/TrialManager/openTrial/getExperimentalDesignName/' + $scope.data.designType).then(function(result) {
									$scope.currentDesignType = [];
									$scope.currentDesignType.id = $scope.data.designType;
									$scope.currentDesignType.name = result.data.name;
								});
							}

						}

						$scope.germplasmDescriptorSettings = TrialManagerDataService.settings.germplasm;
						$scope.data.treatmentFactors = TrialManagerDataService.settings.treatmentFactors.details;
						$scope.data.treatmentFactorsData = TrialManagerDataService.currentData.treatmentFactors.currentData;
						$scope.measurementDetails = {hasMeasurement: studyStateService.hasGeneratedDesign()};
					};

					$scope.isDeleteDesignDisable = function (){
						return !studyStateService.hasGeneratedDesign() || studyStateService.hasListOrSubObs() || studyStateService.hasMeansDataset();
					};

					$scope.deleteDesign = function () {
						if (studyStateService.hasUnsavedData()) {
							showErrorMessage('', 'Please save your data before deleting the design');
							return;
						}
						var deleteMessage = Object.keys(TrialManagerDataService.currentData.treatmentFactors.currentData).length > 0 ? 'With the deletion of the experimental design all observations and Treatment Factors will be lost. Do you want to proceed?' : 'With the deletion of the experimental design all observations will be lost. Do you want to proceed?';
						var modalConfirmDelete = $rootScope.openConfirmModal(deleteMessage, 'Yes','No');
						modalConfirmDelete.result.then(function (shouldContinue) {
							if (shouldContinue) {
								experimentDesignService.deleteDesign().then(
									function (response) {
										showSuccessfulMessage('', 'The design was deleted successfully');
										window.location = '/Fieldbook/TrialManager/openTrial/' + studyContext.studyId;
									}, function (errResponse) {
										showErrorMessage('', errResponse.errors[0].message);
									}
								);
							}
						});
					};

					$scope.totalGermplasmEntryListCount = TrialManagerDataService.specialSettings.experimentalDesign.
							germplasmTotalListCount;

					$scope.data = TrialManagerDataService.currentData.experimentalDesign;

					if (!$scope.data || Object.keys($scope.data).length <= 1) {
						angular.copy({
							totalGermplasmListCount: $scope.totalGermplasmEntryListCount,
							designType: null,
							replicationsCount: null,
							replicationPercentage: null,
							blockSize: null,
							useLatenized: false,
							nblatin: null,
							replicationsArrangement: null,
							rowsPerReplications: null,
							colsPerReplications: null,
							nrlatin: null,
							nclatin: null,
							replatinGroups: '',
							startingPlotNo: 1,
							hasMeasurementData: false, // TODO now the data is deleted after create a new design.
							numberOfBlocks: null
						}, $scope.data);
					}


					if ($scope.data.checkStartingPosition == null) {
						$scope.data.checkStartingPosition = 1;
						$scope.data.checkInsertionManner = '8414';
					}


					TrialManagerDataService.specialSettings.experimentalDesign.data = $scope.data;

					$scope.computeLocalData();

					$scope.replicationsArrangementGroupsOpts = {
						1: 'In a single column',
						2: 'In a single row',
						3: 'In adjacent columns'
					};

					$scope.disableDesignTypeSelect = function () {
						return !!$scope.measurementDetails && $scope.measurementDetails.hasMeasurement;
					};

					$scope.onSwitchDesignTypes = function(newId) {
						if (newId !== '') {

							$scope.currentDesignType = TrialManagerDataService.getDesignTypeById(newId, $scope.designTypes);
							$scope.currentParams = EXPERIMENTAL_DESIGN_PARTIALS_LOC + $scope.currentDesignType.params;
							$scope.data.designType = $scope.currentDesignType.id;
							TrialManagerDataService.currentData.experimentalDesign.designType = $scope.data.designType;

							if (DESIGN_TYPE.ENTRY_LIST_ORDER === $scope.data.designType ) {
								$scope.refreshDesignDetailsForELODesign();
							} else if (DESIGN_TYPE.AUGMENTED_RANDOMIZED_BLOCK === $scope.data.designType ) {
								$scope.refreshDesignDetailsForAugmentedDesign();
							} else if (DESIGN_TYPE.P_REP === $scope.data.designType ) {
								$scope.refreshDesignDetailsForPRepDesign();
							}

						} else {
							$scope.currentDesignType = null;
							$scope.data.designType = '';
							$scope.currentParams = '';
						}

					};

					// on click generate design button
					$scope.generateDesign = function() {
						if (studyStateService.hasUnsavedData()) {
							showErrorMessage('', 'Please save your data before generating the design');
							return;
						}

						if ($scope.data.designType !== DESIGN_TYPE.ENTRY_LIST_ORDER) {
							experimentDesignService.getBVDesignLicense().then(function (response) {
								// we need only the first license
                                var licenseExpiryDays = response.data[0].expiryDays;
								if (licenseExpiryDays <= 0) {
									showErrorMessage($.fieldbookMessages.errorServerError, $.experimentDesignMessages.bvLicenseExpired);
									return;
								} else if (licenseExpiryDays <= 30) {
									$scope.showConfirmDialog($.experimentDesignMessages.bvLicenseExpiring.replace('{0}', licenseExpiryDays)).then(function() {
										$scope.continueGeneration();
									});
								} else {
									$scope.continueGeneration();
								}
							}, function(errResponse) {
								showErrorMessage($.fieldbookMessages.errorServerError, $.experimentDesignMessages.bvLicenseGenericError);
							});
						} else {
							$scope.continueGeneration();
						}
					};

					$scope.continueGeneration = function() {
						if (!$scope.doValidate()) {
							return;
						}

						TrialManagerDataService.performDataCleanup();

						var experimentDesignInput = angular.copy($scope.data);
						// transform ordered has of treatment factors if existing to just the map
						if (experimentDesignInput && experimentDesignInput.treatmentFactors) {
							experimentDesignInput.treatmentFactors = $scope.data.treatmentFactors.vals();
						}
						experimentDesignInput.environments = TrialManagerDataService.currentData.instanceInfo.instances;
						experimentDesignInput.trialSettings = TrialManagerDataService.currentData.trialSettings;

						$scope.openSelectEnvironmentToGenerateModal(experimentDesignInput);
					};

					$scope.openSelectEnvironmentToGenerateModal = function(experimentDesignInput) {
						studyInstanceService.getStudyInstances().then(function(instances) {
							var hasStudyInstanceForGeneration = false;
							$.each(instances, function (key, value) {
								if(value['canBeDeleted']) {
									hasStudyInstanceForGeneration = true;
								}
							});
							if(hasStudyInstanceForGeneration) {
								$uibModal.open({
									templateUrl: '/Fieldbook/static/angular-templates/generateDesign/selectEnvironmentModal.html',
									controller: "generateDesignCtrl",
									size: 'md',
									resolve: {
										experimentDesignInput: function () {
											return experimentDesignInput;
										},
										studyInstances: function () {
											return instances;
										}
									},
									controllerAs: 'ctrl'
								});
							} else {
								showAlertMessage('', 'All instances cannot be regenerated due to internal validations (presence of ' +
									'samples, inventory transactions, subobservations, means data or advance/cross list).');
							}
						});
					};

					$scope.showConfirmDialog = function(message) {

						var deferred = $q.defer();

						bootbox.dialog({
							message: message,
							closeButton: false,
							onEscape: false,
							buttons: {
								ok: {
									label: Messages.OK,
									className: 'btn-primary',
									callback: function() {
										deferred.resolve(true);
									}
								}
							}
						});

						return deferred.promise;
					};

					// Register designImportGenerated handler that will activate when an importDesign is generated
					$scope.$on('designImportGenerated', function() {
						var summaryPromise = $http.get('/Fieldbook/DesignImport/getMappingSummary');
						var designTypeDetailsPromise = $http.get('/Fieldbook/DesignImport/getCustomImportDesignTypeDetails');

						$q.all([summaryPromise, designTypeDetailsPromise]).then(function(results) {
							$scope.applicationData.importDesignMappedData = results[0].data;
							$scope.currentDesignType.templateName = results[1].data.templateName;
						});
					});

					$scope.showConfirmResetDesign = function() {

						var deferred = $q.defer();

						bootbox.dialog({
							title: Messages.DESIGN_IMPORT_PRESET_DESIGN_CHANGE_DESIGN,
							message: Messages.DESIGN_IMPORT_CHANGE_DESIGN_DESCRIPTION_STUDY,
							closeButton: false,
							onEscape: false,
							buttons: {
								yes: {
									label: Messages.YES,
									className: 'btn-primary',
									callback: function() {
										deferred.resolve(true);
									}
								},
								no: {
									label: Messages.NO,
									className: 'btn-default',
									callback: function() {
										deferred.reject(false);
									}
								}
							}
						});

						return deferred.promise;
					};

					$scope.resetExperimentalDesign = function() {

						$scope.showConfirmResetDesign().then(function(result) {
							$scope.resetExperimentalDesignRelatedVariables();
						});
					};

					$scope.resetExperimentalDesignRelatedVariables = function() {
						// the following reset the data used for the experimental design, allowing the user to select another design again
						$scope.applicationData.isGeneratedOwnDesign = false;
						$scope.currentDesignType = null;
						$scope.applicationData.importDesignMappedData = null;
						$scope.data.designType = '';
					};

					$scope.$on('importedDesignReset', function() {
						$scope.resetExperimentalDesignRelatedVariables();
					});

					$scope.toggleDesignView = function() {
						var selectedExperimentDesignType = TrialManagerDataService.getDesignTypeById($scope.data.designType, $scope.designTypes);
						if($scope.data.designType !== null && !selectedExperimentDesignType) return true;
						return ($scope.applicationData.isGeneratedOwnDesign
							|| ($scope.data.designType !== null
							&& $scope.data.designType !== ''
							&& selectedExperimentDesignType.name === 'Custom Import Design')
							);
					};

					$scope.isImportedDesign = function() {
						var selectedExperimentDesignType = TrialManagerDataService.getDesignTypeById($scope.data.designType, $scope.designTypes);
						if(!selectedExperimentDesignType) return true;
						return $scope.data.designType != null
							&& $scope.data.designType !== ''
							&& selectedExperimentDesignType.name === 'Custom Import Design';
					};

					$scope.isBVDesign = function() {
						var selectedExperimentDesignType = TrialManagerDataService.getDesignTypeById($scope.data.designType, $scope.designTypes);
						if(!selectedExperimentDesignType) return false;
						return $scope.data.designType != null
							&& $scope.data.designType !== ''
							&& selectedExperimentDesignType.name !== 'Custom Import Design';
					};

					$scope.showOrHideAdvancedOptions = function (isShown) {
						$scope.settings.showAdvancedOptions[$scope.currentDesignType.id] = isShown;
						$scope.data.useLatenized = isShown;
					};

					$scope.doValidate = function() {

						if(!$scope.validateStartingPlotNo()) {
							showErrorMessage(MESSAGE_DIV_ID, 'Starting plot number must be an integer in range 1 to 99999999.');
							return false;
						}

						switch ($scope.currentDesignType.id) {
							case DESIGN_TYPE.RANDOMIZED_COMPLETE_BLOCK:
							{
								if (!$scope.data.replicationsCount || $scope.expDesignForm.replicationsCount.$invalid) {
									showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[4]);
									return false;
								}

								if (!$scope.settings.treatmentFactors || !TrialManagerDataService.currentData.treatmentFactors.currentData) {
									showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[18]);
									return false;
								}

								var isValidTreatmentVars = TrialManagerDataService.validateAllTreatmentFactorLabels({});
								if (!!isValidTreatmentVars && isValidTreatmentVars.hasError) {
									showErrorMessage(isValidTreatmentVars.customHeader, isValidTreatmentVars.customMessage);
									return false;
								}

								var errorCode = TrialManagerDataService.treatmentFactorDataInvalid();

								if (errorCode === 1) {
									showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[24]);
									return false;
								} else if (errorCode === 2) {
									showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[25]);
									return false;
								}

								break;
							}
							case DESIGN_TYPE.RESOLVABLE_INCOMPLETE_BLOCK:
							{

								if (!$scope.data.replicationsCount || $scope.expDesignForm.replicationsCount.$invalid) {
									showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[5]);
									return false;
								}

								if (!$scope.data.blockSize || $scope.expDesignForm.blockSize.$invalid) {
									showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[8]);
									return false;
								}

								if ($scope.totalGermplasmEntryListCount % $scope.data.blockSize > 0) {
									showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[13]);
									return false;
								}

								// latinized
								if ($scope.data.useLatenized) {
									if ($scope.data.nblatin === null) {
										showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[27]);
										return false;
									}
									if (Number($scope.data.nblatin) >= ($scope.totalGermplasmEntryListCount / $scope.data.blockSize)) {
										showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[11]);
										return false;
									}

									if (Number($scope.data.replicationsArrangement) <= 0) {
										showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[21]);
										return false;
									}
									if (Number($scope.data.replicationsArrangement) === 3) {
										if (!$scope.data.replatinGroups || $scope.expDesignForm.replatinGroups.$invalid) {
											showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[22]);
											return false;
										}

										// validate sum of replatinGroups
										var sum = 0;
										var arrGroups = $scope.data.replatinGroups.split(',');

										for (var i = 0; i < arrGroups.length; i++) {
											sum += Number(arrGroups[i]);
										}

										if (sum !== Number($scope.data.replicationsCount)) {
											showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[12]);
											return false;
										}
									}
								}

								break;
							}
							case DESIGN_TYPE.ROW_COL:
							{
								if (!$scope.data.replicationsCount && $scope.expDesignForm.replicationsCount.$invalid) {
									showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[5]);
									return false;
								}

								if ($scope.data.rowsPerReplications * $scope.data.colsPerReplications !== parseInt($scope.totalGermplasmEntryListCount)) {
									showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[6]);
									return false;
								}

								if ($scope.data.useLatenized) {

									if(Number($scope.data.nrlatin) >= Number($scope.totalGermplasmEntryListCount) / Number($scope.data.rowsPerReplications)) {
										showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[15]);
										return false;
									}

									if(Number($scope.data.nclatin) >= Number($scope.totalGermplasmEntryListCount) / Number($scope.data.colsPerReplications)) {
										showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[16]);
										return false;
									}

									if (Number($scope.data.nrlatin) <= 0 || Number($scope.data.nrlatin) >= Number($scope.data.rowsPerReplications)) {
										showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[14]);
										return false;

									}

									if (Number($scope.data.nclatin) <= 0 || Number($scope.data.nclatin) >= Number($scope.data.colsPerReplications)) {
										showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[17]);
										return false;

									}

									if (Number($scope.data.replicationsArrangement <= 0)) {
										showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[21]);
										return false;
									}

									if (Number($scope.data.replicationsArrangement) === 3) {
										if (!$scope.data.replatinGroups || $scope.expDesignForm.replatinGroups.$invalid) {
											showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[22]);
											return false;
										}

										// validate sum of replatinGroups
										var _sum = 0;
										var _arrGroups = $scope.data.replatinGroups.split(',');

										for (var j = 0; j < _arrGroups.length; j++) {
											_sum += Number(_arrGroups[j]);
										}

										if (_sum !== Number($scope.data.replicationsCount)) {
											showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[12]);
											return false;
										}
									}

								}

								break;
							}
							case DESIGN_TYPE.AUGMENTED_RANDOMIZED_BLOCK: {

								if (!validateNumberOfBlocks()) {
									return false;
								}
								if (!$scope.checkIfTheNumberOfTestEntriesPerBlockIsWholeNumber()) {
									return false;
								}
								if (!validateNumberOfChecks()) {
									return false;
								}
								break;

							}
							case DESIGN_TYPE.ENTRY_LIST_ORDER: {

								if ($scope.germplasmTotalCheckEntriesCount > 0) {
									if ($scope.germplasmTotalTestEntriesCount === 0) {
										showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[33]);
										return false
									}
									if ($scope.data.checkStartingPosition > $scope.germplasmTotalTestEntriesCount) {
										showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[30]);
										return false;
									}
									if ($scope.data.checkStartingPosition < 1) {
										showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[32]);
										return false
									}
									if ($scope.data.checkSpacing > $scope.germplasmTotalTestEntriesCount) {
										showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[29]);
										return false
									}
									if ($scope.data.checkSpacing < 1) {
										showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[31]);
										return false
									}
								} else {
									$scope.data.checkSpacing = '';
								}
								break;
							}
							case DESIGN_TYPE.P_REP:  {

								var largestPossiblePercentage = Math.round((($scope.germplasmTotalTestEntriesCount - 1) / $scope.germplasmTotalTestEntriesCount) * 100);

								// Percentage should be below largestPossiblePercentage. There must be some unreplicated treatments in a partially replicated design.
								if ((!$scope.data.replicationPercentage && $scope.data.replicationPercentage !== 0)  || $scope.expDesignForm.replicationPercentage.$invalid
									|| $scope.data.replicationPercentage > largestPossiblePercentage || $scope.expDesignForm.replicationPercentage.$viewValue === '') {
									showErrorMessage(MESSAGE_DIV_ID, '% of test entries to replicate should be an integer number ' +
										'greater or equal than 0 and less than or equal to ' + largestPossiblePercentage + '. Check entries must be specified if no test entry is replicated.');
									return false;
								}

								if (!$scope.data.replicationsCount || $scope.expDesignForm.replicationsCount.$invalid) {
									showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[4]);
									return false;
								}
								if (!$scope.data.replicationsCount || $scope.expDesignForm.replicationsCount.$invalid) {
									showErrorMessage(MESSAGE_DIV_ID, 'Replication count should be greater than 1');
									return false;
								}

								if (!$scope.data.blockSize || $scope.expDesignForm.blockSize.$invalid) {
									showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[8]);
									return false;
								}

								if ($scope.germplasmNumberOfPlotsPerBlock % 1 !== 0) {
									showErrorMessage(MESSAGE_DIV_ID, 'The block size must be a factor of the number of treatments.');
									return false;
								}

							}

						}

						if (!TrialManagerDataService.applicationData.germplasmListSelected) {
							showErrorMessage(MESSAGE_DIV_ID, EXP_DESIGN_MSGS[26]);
							return false;
						}

						return true;
					};

					$scope.validateStartingPlotNo = function() {
						var validNo = '^(?=.*[1-9].*)[0-9]{1,8}$';

						if (!String($scope.data.startingPlotNo).match(validNo)) {
							return false;
						}
						return true;
					};

					$scope.checkIfTheNumberOfTestEntriesPerBlockIsWholeNumber = function() {
						// Check if the Number of Test entries per block is a whole number
						if ($scope.germplasmNumberOfTestEntriesPerBlock % 1 !== 0) {
							showErrorMessage(MESSAGE_DIV_ID, 'The number of test entries must be divisible by number of blocks.');
							return false;
						}
						return true;
					};

					$scope.showOnlyIfNumberOfBlocksIsSpecified = function() {

						if ($scope.currentDesignType && $scope.currentDesignType.id === DESIGN_TYPE.AUGMENTED_RANDOMIZED_BLOCK) {
							if (!$scope.data.numberOfBlocks && $scope.data.numberOfBlocks !== 0) {
								return false;
							}
							return true;
						}

					};

					$scope.showOnlyIfNumberOfBlockSizeIsSpecified = function() {

						if ($scope.currentDesignType && $scope.currentDesignType.id === DESIGN_TYPE.P_REP) {
							if (!$scope.data.blockSize && $scope.data.blockSize !== 0) {
								return false;
							}
							return true;
						}

					};

					$scope.showOnlyIfNumberOfReplicationsCountIsSpecified = function() {

						if ($scope.currentDesignType && $scope.currentDesignType.id === DESIGN_TYPE.P_REP) {
							if (!$scope.data.replicationsCount && $scope.data.replicationsCount !== 0) {
								return false;
							}
							if (isNaN($scope.germplasmTotalNumberOfPlots)) {
								return false;
							}
							return true;
						}
					};



					$scope.refreshDesignDetailsForAugmentedDesign = function() {
						studyEntryService.getStudyEntriesMetadata().then(function (metadata) {
							$scope.germplasmTotalTestEntriesCount = metadata.testEntriesCount;
							$scope.germplasmTotalCheckEntriesCount = $scope.totalGermplasmEntryListCount - $scope.germplasmTotalTestEntriesCount;
							$scope.germplasmNumberOfTestEntriesPerBlock = $scope.germplasmTotalTestEntriesCount / $scope.data.numberOfBlocks;
							$scope.germplasmNumberOfPlotsPerBlock = $scope.germplasmNumberOfTestEntriesPerBlock + $scope.germplasmTotalCheckEntriesCount;
							$scope.germplasmTotalNumberOfPlots = $scope.germplasmNumberOfPlotsPerBlock * $scope.data.numberOfBlocks;
						});

					}

					$scope.refreshDesignDetailsForELODesign = function() {
						studyEntryService.getStudyEntriesMetadata().then(function (metadata) {
							$scope.germplasmTotalTestEntriesCount = metadata.testEntriesCount;
							$scope.germplasmTotalCheckEntriesCount = $scope.totalGermplasmEntryListCount - $scope.germplasmTotalTestEntriesCount;
						});
					}

					$scope.refreshDesignDetailsForPRepDesign = function() {
						studyEntryService.getStudyEntriesMetadata().then(function (metadata) {
							$scope.germplasmTotalCheckEntriesCount = metadata.nonTestEntriesCount;
							$scope.germplasmTotalTestEntriesCount = $scope.totalGermplasmEntryListCount - $scope.germplasmTotalCheckEntriesCount;
							var noOfTestEntriesToReplicate = Math.round($scope.germplasmTotalTestEntriesCount * ($scope.data.replicationPercentage / 100));
							$scope.germplasmTotalNumberOfPlots = ($scope.germplasmTotalTestEntriesCount - noOfTestEntriesToReplicate) +
								(noOfTestEntriesToReplicate * $scope.data.replicationsCount) +
								($scope.germplasmTotalCheckEntriesCount * $scope.data.replicationsCount);
							$scope.germplasmNumberOfPlotsPerBlock = $scope.germplasmTotalNumberOfPlots / $scope.data.blockSize;
						});
					}

					$scope.showParamsWhenChecksAreSelected = function(designTypeId) {
						return !(designTypeId === DESIGN_TYPE.ENTRY_LIST_ORDER &&
							($scope.germplasmTotalTestEntriesCount === parseInt($scope.totalGermplasmEntryListCount)
								|| $scope.totalGermplasmEntryListCount === 0  || $scope.totalGermplasmEntryListCount === null));
					}

					function validateNumberOfBlocks() {
						if (!$scope.data.numberOfBlocks || $scope.expDesignForm.numberOfBlocks.$invalid) {
							showErrorMessage(MESSAGE_DIV_ID, 'Please specify the number of blocks.');
							return false;
						}
						return true;
					}

					function validateNumberOfChecks() {

						if ($scope.germplasmTotalCheckEntriesCount === 0) {
							showErrorMessage(MESSAGE_DIV_ID, 'Please specify checks in germplasm list before generating augmented design.');
							return false;
						}
						return true;
					}

				}])

			// FILTERS USED FOR EXP DESIGN

			.filter('filterFactors', ['_','DESIGN_TYPE', function(_, DESIGN_TYPE) {
				return function(factorList, designTypeId) {
					var excludeTermIds;

					if (designTypeId === DESIGN_TYPE.RANDOMIZED_COMPLETE_BLOCK) {
						excludeTermIds = [8230, 8220, 8581, 8582, 8842];
					} else if (designTypeId === DESIGN_TYPE.RESOLVABLE_INCOMPLETE_BLOCK) {
						excludeTermIds = [8581, 8582, 8842];
					} else if (designTypeId === DESIGN_TYPE.ROW_COL) {
						excludeTermIds = [8220, 8200, 8842];
					} else if (designTypeId === DESIGN_TYPE.AUGMENTED_RANDOMIZED_BLOCK) {
						excludeTermIds = [8210, 8581, 8582, 8842];
					} else if (designTypeId === DESIGN_TYPE.ENTRY_LIST_ORDER) {
						excludeTermIds = [8210, 8220, 8581, 8582, 8842];
					} else if (designTypeId === DESIGN_TYPE.P_REP) {
						excludeTermIds = [8210, 8581, 8582, 8842];
					}

					return _.filter(factorList, function(value) {
							return !_.contains(excludeTermIds, value);
					});


				};
			}]);

		manageTrialAppModule.controller('generateDesignCtrl', ['experimentDesignInput', 'studyInstances', '$scope', '$rootScope', '$uibModalInstance',
			'experimentDesignService', 'studyContext',
			function (experimentDesignInput, studyInstances, $scope, $rootScope, $uibModalInstance, experimentDesignService, studyContext) {

				var generateDesignCtrl = this;

				$scope.instances = studyInstances;
				$scope.selectedInstances = {};
				$scope.isEmptySelection = false;

				$scope.cancel = function () {
					$uibModalInstance.dismiss();
				};

				$scope.validateSelectedEnvironments = function () {
					experimentDesignInput.trialInstancesForDesignGeneration = generateDesignCtrl.getSelectedInstanceNumbers();

					var environmentsWithMeasurements = [];
					//Check if selected instances has measurement data
					$scope.instances.forEach(function (instance) {
						if($scope.selectedInstances[instance['instanceNumber']] && (instance['hasMeasurements'] || instance['hasFieldmap'])) {
							environmentsWithMeasurements.push(instance['instanceNumber']);
						}
					});
					if(environmentsWithMeasurements.length > 0 ) {
						generateDesignCtrl.showHasMeasurementsWarning(environmentsWithMeasurements);
					} else {
						var environmentsWithDesign = [];
						//Check if selected instances has measurement data
						$scope.instances.forEach(function (instance) {
							if($scope.selectedInstances[instance['instanceNumber']] && instance['hasExperimentalDesign']) {
								environmentsWithDesign.push(instance['instanceNumber']);
							}
						});
						if(environmentsWithDesign.length > 0) {
							generateDesignCtrl.showHasGeneratedDesignWarning(environmentsWithDesign);
						} else {
							generateDesignCtrl.generateDesign();
						}
					}
				};

				generateDesignCtrl.generateDesign = function() {
					experimentDesignService.generateDesign(experimentDesignInput).then(
						function(response) {
							showSuccessfulMessage('', $.experimentDesignMessages.experimentDesignGeneratedSuccessfully);
							window.location = '/Fieldbook/TrialManager/openTrial/' + studyContext.studyId;
						}, function(errResponse) {
							var errorMessage = errResponse.errors[0].message;
							showErrorMessage($.fieldbookMessages.errorServerError, $.fieldbookMessages.errorDesignGenerationFailed + ' ' + errorMessage);
						});
				}

				generateDesignCtrl.getSelectedInstanceNumbers = function () {

					var instanceNumbers = [];

					Object.keys($scope.selectedInstances).forEach(function (instanceNumber) {
						var isSelected = $scope.selectedInstances[instanceNumber];
						if (isSelected) {
							instanceNumbers.push(instanceNumber);
						}
					});

					return instanceNumbers;

				};

				generateDesignCtrl.showHasMeasurementsWarning = function(environmentsWithMeasurements) {
					var deleteMessage = "Observation data and fieldmaps of the following environment(s): " + environmentsWithMeasurements.toString()
						.replace(new RegExp(",", 'g'), ", ") + " will be deleted. Do you want to continue?";
					var modalConfirmDelete = $rootScope.openConfirmModal(deleteMessage, 'Yes','No');
					modalConfirmDelete.result.then(function (shouldContinue) {
						if (shouldContinue) {
							generateDesignCtrl.generateDesign();
						} else {
							$scope.cancel();
						}
					});
				};

				generateDesignCtrl.showHasGeneratedDesignWarning = function(environmentsWithDesign) {
					var deleteMessage = "The following environment(s): " + environmentsWithDesign.toString()
						.replace(new RegExp(",", 'g'), ", ") + " already has observations. Do you want to continue?";
					var modalConfirmDelete = $rootScope.openConfirmModal(deleteMessage, 'Yes','No');
					modalConfirmDelete.result.then(function (shouldContinue) {
						if (shouldContinue) {
							generateDesignCtrl.generateDesign();
						} else {
							$scope.cancel();
						}
					});
				};


			}]);

	}
)();
