import subprocess
import os.path
import shutil

from git import Repo
from os import path
from threading import Thread

repo_url = 'https://github.com/colinrgodsey/step-daemon.git'

class StepdThread(Thread):
  def __init__(self, base_path, logger):
    Thread.__init__(self)

    self.base_path = base_path
    self.logger = logger
    self.running = False

    self.repo_path = os.path.join(self.base_path, 'repo')
    self.jar_path = os.path.join(self.base_path, 'stepd.jar')

  def update_and_build(self):
    pass

  def init_stepd_repo(self):
    self.logger.info("Init stepd at " + self.repo_path)

    repo = Repo.clone_from(repo_url, self.repo_path)
    shutil.copyfile(
      os.path.join(self.repo_path, 'config.conf.example'),
      os.path.join(self.base_path, 'config.conf')
    )

    return repo

  def build_failed(self):
    self.logger.error("Build failed!")

  def sbt_operation(self, args):
    sbt_path = os.path.join('sbt-files', 'sbt-launch.jar')
    process = subprocess.Popen(['java', '-jar', sbt_path] + args,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE,
                               universal_newlines=True,
                               cwd=self.repo_path)

    while True:
      # for output in process.stderr.readlines():
      #  self.logger.error('%s', output.strip())

      self.logger.info('SBT: %s', process.stdout.readline().strip())

      return_code = process.poll()
      if return_code is not None:
        for output in process.stdout.readlines():
          self.logger.info('SBT: %s', output.strip())
        break

    return return_code is 0

  def run_server(self):
    self.running = True

    jvm_args = ['-Xmx64M', '-XX:+UseG1GC', '-XX:ParallelGCThreads=4',
                '-XX:ConcGCThreads=2', '-XX:MaxGCPauseMillis=5']
    process = subprocess.Popen(['java', '-jar', 'stepd.jar'] + jvm_args,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE,
                               universal_newlines=True,
                               cwd=self.base_path)

    while True:
      self.logger.info('Server: %s', process.stdout.readline().strip())

      return_code = process.poll()
      if return_code is not None:
        for output in process.stdout.readlines():
          self.logger.info('Server: %s', output.strip())
        break

  def run(self):
    self.logger.info("Checking for stepd updates...")

    new_checkout = False
    if path.exists(self.repo_path):
      repo = Repo(self.repo_path)
    else:
      repo = self.init_stepd_repo()
      new_checkout = True

    old_commit = repo.active_branch.commit

    for fetch_info in repo.remotes.origin.fetch():
      self.logger.debug("Remote at %s, local was at %s" % (repo.active_branch.commit, old_commit))
      if fetch_info.commit != old_commit or new_checkout:
        self.logger.info("Changes detected, triggering build...")
        repo.active_branch.set_reference(repo.remotes.origin.refs.master)
      else:
        self.logger.info("Build is already up to date.")
        self.run_server()
        return

    if self.sbt_operation(['clean', 'server/assembly']):
      jar_path_target = os.path.join(self.repo_path, 'server', 'target',
                                     'scala-2.13', 'print-server-jvm-assembly-0.1.0-SNAPSHOT.jar')

      self.logger.info('Copying jar to ' + str(self.jar_path))
      shutil.copyfile(jar_path_target, self.jar_path)

      self.logger.info('Cleaning up')
      self.sbt_operation(['clean'])

      self.run_server()
    else:
      self.build_failed()

