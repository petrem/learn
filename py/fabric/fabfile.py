from pprint import pprint
import time

from fabric.api import task, env, hosts, execute, parallel, runs_once, run
from fabric.decorators import roles
from fabric.tasks import Task


# class CustomTask(Task):
#     def __init__(self, func, myarg, *args, **kwargs):
#         super(CustomTask, self).__init__(*args, **kwargs)
#         self.func = func
#         self.myarg = myarg

#     def run(self, *args, **kwargs):
#         pprint("running task %s (%s):" % (env.command, self.func.func_name))
#         pprint("host: %s" % env.host_string)
#         pprint("all hosts: %s" % env.all_hosts)
#         return self.func(*args, **kwargs)


# @task(task_class=CustomTask, myarg='value', alias='at')
# def actual_task():
#     pass


# def callme():
#     return


# env.roledefs = {
#     'foo': None,
#     'bar': None,
# }


# @roles('foo')
# @task
# def webserver():
#     pprint("running webserver fn on %s" % env.host)
#     run('hostname')


# @roles('bar')
# @task
# def dbserver():
#     pprint("running dbserver fn on %s" % env.host)
#     run('hostname')


# @roles('foo', 'bar')
# @task
# def common():
#     pprint("running common fn on %s" % env.host)
#     run('hostname')


# @hosts('localhost')
# @task
# def special():
#     pprint("running special fn on %s" % env.host)
#     run('hostname')


# config = {
#     'foo': ['jean-luc.local'],
#     'bar': ['192.168.1.53']
# }


# for role in config:
#     env.roledefs[role] = config[role]


# env.roledefs = {
#     'foo': ['dotest-01.local'],
#     'bar': ['dotest-02.local'],
# }

env.roledefs = {
    'foo': {'hosts': [], 'bla': 'foo'},
    'baz': {'hosts': [], 'bla': 'baz'},
    'bar': {'hosts': [], 'bla': 'bar'}}

env.linewise = True


@task
@roles('foo', 'bar', 'baz')
@parallel
def task1():
    pprint('task 1 start on %s' % env.host)
    if env.host == 'dotest-02.local':
        time.sleep(5)
    pprint("hosts:");pprint(env.hosts, indent=4)
    pprint("roles:");pprint(env.roles, indent=4)
    pprint("roledefs:");pprint(env.roledefs, indent=4)
    time.sleep(5)
    pprint('task 1 stop')


class Task2(Task):
    name = 'task2'

    def __init__(self, env):
        self.env = env

    def run(self, arg):
        pprint('task 2 start with %s' % arg)
        time.sleep(5)
        pprint("hosts:");pprint(self.env.hosts, indent=4)
        pprint("roles:");pprint(self.env.roles, indent=4)
        pprint("roledefs:");pprint(self.env.roledefs, indent=4)
        pprint('task 2 stop')


t2 = Task2(env)  # NOQA


@task
@runs_once
@hosts('localhost')
def all():
    env.roledefs['foo']['hosts'] = ['dotest-01.local']
    env.roledefs['bar']['hosts'] = ['dotest-02.local']
    execute('task1')
    execute('task2', 'argfoo', hosts={'h1': 1, 'h2': 2})


env.forward_agent = True
env.use_ssh_config = True

@task
def clone():
    run('hg clone ssh://hg@cmedbitbucket.org/cmedtechnology/iceterraform')
