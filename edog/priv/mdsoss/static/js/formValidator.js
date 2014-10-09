/**
 * @author ming
 */
$(document).ready(function(){

/* 设置默认属性 */
$.validator.setDefaults({
    submitHandler: function(form) {
        form.submit();
    }
});

function Trim(str){
    return str.replace(/(^\s*)|(\s*$)/g, "");
}

// 字符验证
jQuery.validator.addMethod("stringCheck", function(value, element) {
    return this.optional(element) || /^[\u4e00-\u9fa5\w]+$/.test(value);
}, "只能包括英文字母、中文、数字和下划线");

//一个正则表达式，只含有汉字、数字、字母、下划线不能以下划线开头和结尾：
//^(?!_)(?!.*?_$)[a-zA-Z0-9_\u4e00-\u9fa5]+$

// 字符验证
jQuery.validator.addMethod("stringENCheck", function(value, element) {
    return this.optional(element) || /^[A-Za-z0-9\s-]+$/.test(value);
}, "只能包括英文字母或数字或横线");

// 中文字两个字节
jQuery.validator.addMethod("byteRangeLength", function(value, element, param) {
    var length = value.length;
    for(var i = 0; i < value.length; i++){
        if(value.charCodeAt(i) > 127){
        length++;
        }
    }
    return this.optional(element) || ( length >= param[0] && length <= param[1] );
}, "请确保输入的值在3-15个字节之间(一个中文字算2个字节)");

// 身份证号码验证
jQuery.validator.addMethod("isIdCardNo", function(value, element) {
    return this.optional(element) || isIdCardNo(value);
}, "请正确输入您的身份证号码");

// 手机号码验证
jQuery.validator.addMethod("isMobile", function(value, element) {
    var length = value.length;
    var mobile = /^(((1[0-9]{2})|(1[0-9]{2}))+\d{8})$/;
    return this.optional(element) || (length == 11 && mobile.test(value));
}, "请正确填写您的手机号码");

// 电话号码验证
jQuery.validator.addMethod("isTel", function(value, element) {
	var tel = /^\d{3}-\d{8}|\d{4}-\d{7}$/
    //var tel = /^\d{3,4}-?\d{7,9}$/;    //电话号码格式010-12345678
    return this.optional(element) || (tel.test(value));
}, "请正确填写您的电话号码");

// 联系电话(手机/电话皆可)验证
jQuery.validator.addMethod("isPhone", function(value,element) {
    var length = value.length;
    var mobile = /^(((13[0-9]{1})|(15[0-9]{1}))+\d{8})$/;
    //var tel = /^\d{3,4}-?\d{7,9}$/;
    var tel = /^\d{3}-\d{8}|\d{4}-\d{7}$/
    return this.optional(element) || (tel.test(value) || mobile.test(value));

}, "请正确填写您的联系电话");

// 邮政编码验证
jQuery.validator.addMethod("isZipCode", function(value, element) {
    var tel = /^[0-9]{6}$/;
    return this.optional(element) || (tel.test(value));
}, "请正确填写您的邮政编码");

//开始验证
$('#submitForm').validate({
    /* 设置验证规则 */
    rules: {
    	select_vmimg: {
    		required: true,
    	},
        alias: {
            required:true,
            stringCheck:true,
            byteRangeLength:[3,15]
        },
        company: {
            required:true,
            stringCheck:true,
            byteRangeLength:[3,30]
        },
        address:{
            required:true,
            stringCheck:true,
            byteRangeLength:[3,100]
        },
        telephone:{
            required:true,
            isPhone:true
        },
        contact_person: {
            required:true,
            stringCheck:true,
            byteRangeLength:[3,15]
        },
        cellphone: {
            required:true,
            isMobile:true
        },
        email:{
            required:true,
            email:true
        },

        capacity:{
            required:true,
            number:true,
            max: 5000,
            min: 1
        },
        vd_alias: {
            required:true,
            stringCheck:true,
            byteRangeLength:[3,15]
        },

        cpu_cores:{
            required:true,
            number:true,
            max: 4,
            min: 1
        },
        //mem_capacity:{
        //    required:true,
        //    number:true,
        //    max: 16,
        //    min: 1
        //},
        virtualNetworkCard_num:{
            required:true,
            number:true,
            max: 4,
            min: 1
        },
        vm_name: {
            required:true,
            stringENCheck:true,
            byteRangeLength:[3,15]
        },
        pm_selected: {
        	required:true
        }
    },

    /* 设置错误信息 */
    messages: {
    	select_vmimg: {
    		required: "请选择虚拟机镜像",
    	},
    	alias: {
            required: "请输入标识名称",
            stringCheck: "由英文字母、中文、数字或下划线组成",
            byteRangeLength: "标识名称须大于3个字符，并小于15个字符"
        },
        company: {
            required: "请输入公司全称",
            stringCheck: "由英文字母、中文、数字或下划线组成",
            byteRangeLength: "公司全称须大于3个字符，并小于30个字符"
        },
        address:{
            required: "请输入公司地址",
            stringCheck: "由英文字母、中文、数字或下划线组成",
            byteRangeLength: "请详实您的联系地址以便于我们联系您"
        },
        telephone:{
            required: "请输入公司电话号码",
            isPhone: "请按格式输入有效的固定电话号码"
        },
        contact_person: {
            required: "请输入联系人姓名",
            stringCheck: "由英文字母、中文、数字或下划线组成",
            byteRangeLength: "联系人姓名须大于3个字符，并小于15个字符"
        },
        cellphone: {
            required: "请输入联系人移动电话号码",
            isMobile: "请输入有效的移动电话号码"
        },
        email:{
            required: "请输入email地址",
            email: "请输入有效的email地址"
        },

        capacity:{
        	required: "请输入容量信息",
        	number: "请输入有效数字",
        	max: "请输入允许范围内的容量",
        	min: "请输入允许范围内的容量"
        },

        cpu_cores:{
            required: "请输入CPU内核个数",
            number: "请输入有效数字",
            max: "CPU内核个数须大于等于1，小于等于4",
            min: "CPU内核个数须大于等于1，小于等于4"
        },
        //mem_capacity:{
        //    required: "请输入内存容量",
        //    number: "请输入有效数字",
        //    max: "内存容量须大于等于1，小于等于16",
        //    min: "内存容量须大于等于1，小于等于16"
        //},
        vm_name: {
            required: "请输入标识名称",
            stringENCheck: "由英文字母或数字或横线组成",
            byteRangeLength: "标识名称须大于3个字符，并小于15个字符"
        },
    },

    /* 设置验证触发事件 */
    focusInvalid: false,
    onkeyup: false,

    /* 设置错误信息提示DOM */
    errorPlacement: function(error, element) {
        error.appendTo( element.parent());
    },

});

});
