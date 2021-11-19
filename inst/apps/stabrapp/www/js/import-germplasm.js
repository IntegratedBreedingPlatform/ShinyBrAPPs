/* global angular, germplasmDataTable, BMS, setSpinnerMaxValue, noStudyGermplasmList */
/* global entryNoShouldBeInRange, plotNoShouldBeInRange, showAlertMessage, showInvalidInputMessage */
(function() {
	'use strict';

	if (typeof window.ImportGermplasm === 'undefined') {
		window.ImportGermplasm = {
			initialize: function(dataGermplasmList) {
				var $noGermplasmListIndicator = $('.noGermplasmListIndicator');

				var gpListItemsClass = '.germplasm-list-items',
					gpListDataTblClass = '.germplasm-list-data-table';

				germplasmDataTable = new BMS.Fieldbook.TrialGermplasmListDataTable(gpListItemsClass, gpListDataTblClass, dataGermplasmList);
				if ($noGermplasmListIndicator.length !== 0 && hasGeneratedDesign()) {
					$noGermplasmListIndicator.html(noStudyGermplasmList);
				}
			},
		};
	}
})();
