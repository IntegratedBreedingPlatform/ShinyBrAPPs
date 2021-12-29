var StudyCustomExportReports = {
	customReports: [],
	showReports: function(studyId) {
		'use strict';
		if ($('#exportStudyModal').data('custom-report-loaded') !== '1') {
			$('#exportStudyModal').data('custom-report-loaded', '1');
			$.ajax({
				url: '/Fieldbook/ExportManager/custom/' + studyId + '/reports',
				type: 'GET',
				data: '',
				cache: false,
				success: function(data) {
					StudyCustomExportReports.customReports = data;
					$('#exportType').select2('destroy');
					for (var i = 0 ; i < data.length ; i++) {
						$('#exportType').append(new Option(data[i].code + ' - ' + data[i].name, data[i].code));
					}
					if (data.length != 0) {
						$('.report-type-section ').removeClass('col-xs-4').removeClass('col-md-4');
						$('.report-type-section ').addClass('col-xs-7').addClass('col-md-7');
					}
					$('#exportType').select2({minimumResultsForSearch: 20});
				}
			});
		}
	},
	isCustomReport: function() {
		for (var i = 0 ; i < StudyCustomExportReports.customReports.length; i++) {
			if (StudyCustomExportReports.customReports[i].code == $('#exportType').val()) {
				return true;
			}
		}
		return false;
	},
	doExport: function() {
		'use strict';
		var studyId = 0;
		if ($('#browser-studies').length !== 0) {
			// Meaning we are on the landing page
			studyId = getCurrentStudyIdInTab();
		}else if ($('#createTrialMainForm #studyId').length === 1) {
				studyId = ($('#createTrialMainForm #studyId').val());
		}

		$.ajax('/Fieldbook/ExportManager/export/custom/report', {
			headers: {
				'Accept': 'application/json',
				'Content-Type': 'application/json'
			},
			data: JSON.stringify({
				'customReportCode': $('#exportType').val(),
				'studyExportId': studyId,
			}),
			type: 'POST',
			dataType: 'text',
			success: function(data) {
				showExportResponse(data);
			}
		});
	}
};
