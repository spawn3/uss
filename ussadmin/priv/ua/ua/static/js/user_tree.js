$(document).ready(function () {
		$("#demo")
		.jstree({ 
			"plugins" : [ "themes", "json_data", "ui", "crrm", "cookies", "dnd", "search", "types", "hotkeys", "contextmenu" ],
			"json_data" : { 
			"ajax" : {
			"url" : "/user/tree",
			"data" : function (n) { 
			return { 
			"operation" : "get_children", 
			"id" : n.attr ? n.attr("id").replace("node_","") : "null_-1_super" 
			}; 
			}
			}
			},
			"search" : {
				"ajax" : {
					"url" : "/user/tree",
					"data" : function (str) {
						return { 
							"operation" : "search", 
							"search_str" : str 
						}; 
					}
				}
			},
			"types" : {
				"max_depth" : -2,
				"max_children" : -2,
				"types" : {
					"default" : {
						"valid_children" : "none",
						"icon" : {
							"image" : "/static/img/user.png"
						}
					},
					"user" : {
						"valid_children" : "none",
						"icon" : {
							"image" : "/static/img/user.png"
						}
					},
					"group" : {
						"valid_children" : ["default", "group", "user" ],
						"icon" : {
							"image" : "/static/img/group.png"
						}
					},
				}
			},
			"themes": {
					"theme":"default",	
					"url":"/static/jsTree.v.1.0rc2/themes/default/style.css",
					"dots": true,
					"icons":true,
			},
			"contextmenu": {
				"items"	: {
					"create": {
						"label": "Create",
						"action": function(obj) {},
						"_disabled"         : false,     // clicking the item won't do a thing
						"_class"            : "class",  // class is applied to the item LI node
						"separator_before"  : false,    // Insert a separator before the item
						"separator_after"   : true,     // Insert a separator after the item
						"icon"              : false,
						"submenu"           : {
							/* Collection of objects (the same structure) */
							"create group": {
								"label": "Create group",
								"action": function(obj) {
									if ($(obj).attr("id").split("_")[3] != "group"){
										alert("只能在用户组下面添加组或用户。");	
										return false;
									} 
									$("#addUserDiv").hide();
									$("#addGroupDiv").show();
									$(obj).children("a").trigger("click");
								},
								"_disabled"         : false,     // clicking the item won't do a thing
								"_class"            : "class",  // class is applied to the item LI node
								"separator_before"  : false,    // Insert a separator before the item
								"separator_after"   : true,     // Insert a separator after the item
								"icon"              : false,
							},
							"create user": {
								"label": "Create user",
								"action": function(obj) {
									if ($(obj).attr("id").split("_")[3] != "group"){
										alert("只能在用户组下面添加组或用户。");	
										return false;
									}
									$("#addGroupDiv").hide();
									$("#addUserDiv").show();
									$(obj).children("a").trigger("click");
								},
								"_disabled"         : false,     // clicking the item won't do a thing
								"_class"            : "class",  // class is applied to the item LI node
								"separator_before"  : false,    // Insert a separator before the item
								"separator_after"   : true,     // Insert a separator after the item
								"icon"              : false,
							},
						},
					},
				},

			},
			"ui" : {
				"initially_select" : [ "node_4" ]
			},
			"core" : { 
				"initially_open" : [ "node_2" , "node_3" ] 
			},

		})
.bind("create.jstree", function (e, data) {
		$.post(
			"/user/tree", 
			{ 
			"operation" : "create_node", 
			"id" : data.rslt.parent.attr("id").replace("node_",""), 
			"position" : data.rslt.position,
			"title" : data.rslt.name,
			"type" : data.rslt.obj.attr("rel")
			}, 
			function (r) {
			if(r.status) {
			$(data.rslt.obj).attr("id", "node_" + r.id);
			}
			else {
			$.jstree.rollback(data.rlbk);
			}
			}
			,"json"
			);
})
.bind("remove.jstree", function (e, data) {
		data.rslt.obj.each(function () {
			$.ajax({
async : false,
type: 'POST',
url: "/user/tree",
dataType: "json",
data : { 
"operation" : "remove_node", 
"id" : this.id.replace("node_","")
}, 
success : function (r) {
if(!r.status) {
data.inst.refresh();
}
}
});
			});
		})
.bind("rename.jstree", function (e, data) {
		$.post(
			"/user/tree", 
			{ 
			"operation" : "rename_node", 
			"id" : data.rslt.obj.attr("id").replace("node_",""),
			"title" : data.rslt.new_name
			}, 
			function (r) {
			if(!r.status) {
			$.jstree.rollback(data.rlbk);
			alert(r.msg);
			}
			}
			,"json"
			);
		})
.bind("move_node.jstree", function (e, data) {
		data.rslt.o.each(function (i) {
			$.ajax({
async : false,
type: 'POST',
url: "/user/tree",
dataType: "json",
data : { 
"operation" : "move_node", 
"id" : $(this).attr("id").replace("node_",""), 
"ref" : data.rslt.np.attr("id").replace("node_",""), 
"position" : data.rslt.cp + i,
"title" : data.rslt.name,
"copy" : data.rslt.cy ? 1 : 0
},
success : function (r) {
if(!r.status) {
$.jstree.rollback(data.rlbk);
alert(r.msg)
}
else {
$(data.rslt.oc).attr("id", "node_" + r.id);
if(data.rslt.cy && $(data.rslt.oc).children("UL").length) {
	data.inst.refresh(data.inst._get_parent(data.rslt.oc));
}
}
$("#analyze").click();
}
});
});
});
//控制上面的一行菜单
		$("#mmenu input").click(function () {
			switch(this.id) {
			case "add_default":
			case "add_group":
			$("#demo").jstree("create", $(".jstree-clicked"), "last", { "attr" : { "rel" : this.id.toString().replace("add_","")}});
			break;
			case "search":
			$("#demo").jstree("search", document.getElementById("text").value);
			break;
			case "text": break;
			default:
			$("#demo").jstree(this.id);
			break;
			}
			});


//控制添加group, user.
		$("#demo a").livequery("click",function(){
			var id = ($(this).parent().attr("id"));	
			if ($(this).parent().attr("rel") == "group"){
			//首先去掉input的不可编辑状态，值改变后，恢复不可编辑状态。
			$("#userGroup").removeAttr("readonly");
			$("#groupGroup").removeAttr("readonly");
			$(".userInput").each(function(){
				$(this).val("");
				//					alert("here userInput.");		
				});
			$("#userGroup").val(id);
			$("#groupGroup").val(id);
			$("#userGroup").attr("readonly", "true");
			$("#groupGroup").attr("readonly", "true");
			}
			//下面这两行首先去掉原来的颜色，在给新选择的元素上色。
			$(".jstree-clicked").removeClass("jstree-clicked");
			$(this).addClass("jstree-clicked");
			return false;
		});

		$("#addGroup").click(function() {
				//获取要添加用户组的信息，保存在group里面。
				var group = new Array;
				var i = 0;
				$("#addGroupDiv input").slice(0, 3).each(function(){
					group[i] = $(this).val() ;
					i++;
					});
				var groupName = group[0];
				var groupGroup = group[1];//父节点的id值。
				var groupPermis = group[2];
				// 发送待添加组的信息到服务器端。
				$.post(
					"/user/tree", 
					{ 
					"operation" : "create_node", 
					"position" : "last",
					"title" : groupName,
					"type" : "group",
					"id": groupGroup.replace("node_", ""),
					"permis": groupPermis,
					}, 
					function (r) {
					if(r.status) {
					//alert(r.status);
					//alert(r.id);
					$("#demo").jstree("create", $(".jstree-clicked"), "last", { "data":groupName, "state":"open", "attr" : { "rel" : "group", "id":r.id} }, groupName);
					$("#addGroupDiv").hide();
					}
					else {
					alert(r.msg);
					}
					}
				,"json"
					);
		});

		$("#addUser").click(function() {
				//获取要添加用户组的信息，保存在group里面。
				var user = new Array;
				var i = 0;
				$("#addUserDiv input").slice(0, 4).each(function(){
					user[i] = $(this).val() ;
					i++;
					});
				var userName = user[0];
				var userGroup = user[1];//父节点的id值。
				var userPassword1 = user[2];
				var userPassword2 = user[3];
				if ( userPassword1 == "" ){
				alert("密码不能为空");
				return false;
				}
				if ( userPassword1 != userPassword2 ){
				alert("两次密码输入的不一样。");
				return false;
				}
				else{
					var userPassword = userPassword1;
				}
				// 发送待添加组的信息到服务器端。
				$.post(
						"/user/tree", 
						{ 
						"operation" : "create_node", 
						"position" : "last",
						"title" : userName,
						"type" : "user",
						"id": userGroup.replace("node_", ""),
						"password": userPassword,
						}, 
						function (r) {
						if(r.status) {
						$("#demo").jstree("create", $(".jstree-clicked"), "last", { "data":userName, "attr" : { "rel" : "user", "id":r.id} }, userName);
						$("#addUserDiv").hide();
						}
						else {
						alert(r.msg);
						}
						}
				,"json"
					);
		});

});	
