/*
Loading throbber.
*/
function preload(arrayOfImages) {
    $(arrayOfImages).each(function () {
        $('<img />').attr('src',this).appendTo('body').css('display','none');
    });
}

var spinnerImageSrc = "/Fieldbook/static/img/loading-animation.gif";
preload([spinnerImageSrc]);

window.Spinner = (function() {
    'use strict';
    function startThrobber() {
    	$.blockUI({message:"<img style='padding-top: 45px' src='" + spinnerImageSrc + "'/>",
			css:{background:"#e9e9e9",width:"150px",height:"150px",left:"50%",border:"none",margin:"0px 0px -100px -100px",'border-radius':"15px"},
            overlayCSS: { backgroundColor: '#fff', opacity: 0.6,cursor: 'wait'}
        });
	}
	return {
		play: function() {
			startThrobber();	
			this.is_on=true;
		},
		// This method is intended to be called when your asynchronous method has returned (e.g. you have hit the server and come back)
		stop: function() {
			// Stop the throbber if it has already started playing.
			$.unblockUI();
			this.is_on=false;
		},
		
		toggle:function(){
			if(this.is_on){
				this.stop();
			} else{
				this.play();
			}
		}
	};

}());
