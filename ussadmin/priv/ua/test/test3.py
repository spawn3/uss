#!/usr/bin/python
#-*- coding: utf-8 -*-

class Permission:
    def __init__(self):
        pass
    def needUserPermission(self, function):
        def new_func(*args, **kwargs):
            obj = args[0]
            if obj.user.hasUserPermission():
                ret = function(*args, **kwargs)
            else:
                ret = "No User Permission"
            return ret
        return new_func

    def needAdminPermission(self, function):
        def new_func(*args, **kwargs):
            obj = args[0]
            if obj.user.hasAdminPermission():
                ret = function(*args, **kwargs)
            else:
                ret = "No Admin Permission"
            return ret
        return new_func

permission = Permission()

class Action:
    def __init__(self, name):
        self.user = UserService.newUser(name)

    @permission.needUserPermission           #需要用户权限
    def listAllPoints(self):
        return "TODO: do real list all points"

    @permission.needAdminPermission        #需要管理员权限
    def setup(self):
        return "TODO: do real setup"

if __name__ == "__main__":
    action = Action('user')
    print action.listAllPoints()     #将会执行真正的业务代码
    print action.setup()              #将会返回“No Admin Permission”