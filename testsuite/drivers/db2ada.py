from e3.fs import cp
from e3.testsuite.driver import TestDriver
from e3.testsuite.process import check_call
from e3.testsuite.result import TestStatus
from e3.fs import mkdir
from drivers import gprbuild
import os


class DB2AdaTestDriver(TestDriver):
    """Driver with db2ada integration

    In order to declare a test:

    1- Create a directory with a test.yaml inside. driver should be set to
       "db2ada"
    2- Add test sources in that directory
    3- Add a main called test.adb that use support/test_assert.ads package.
    4- In test.yaml, under the "db2ada" add the list of parameter for
       gnatcoll_db2ada

    Example of test.yaml:

        description: SQL test 1
        db2ada:
          - "-api=DB"
          - "-dbmodel=descr.txt"
        driver: db2ada
    """

    def add_test(self, dag):
        """Declare test workflow.

        The workflow is the following::

            db2ada --> build --> check status

        :param dag: tree of test fragment to amend
        :type dag: e3.collection.dag.DAG
        """
        self.add_fragment(dag, 'db2ada')
        self.add_fragment(dag, 'build', after=['db2ada'])
        self.add_fragment(dag, 'check_run', after=['build'])

        if 'test_exe' not in self.test_env:
            self.test_env['test_exe'] = 'obj/test'

    def db2ada(self, previous_values):
        """Run db2ada."""
        mkdir(self.test_env['working_dir'])
        db2ada_args = []

        # Compute db2ada arguments
        for value in self.test_env.get('db2ada', []):
            if value.startswith('-dbmodel='):
                dbmodel = value.split('=', 1)[1]
                dbmodel = os.path.join(self.test_env['test_dir'], dbmodel)
                db2ada_args.append('-dbmodel=%s' % dbmodel)
            else:
                db2ada_args.append(value)

        check_call(self, ['gnatcoll_db2ada'] + db2ada_args)

    def build(self, previous_values):
        """Build fragment."""
        return gprbuild(self, gcov=self.env.gcov,
                        components=self.env.components)

    def check_run(self, previous_values):
        """Check status fragment."""
        if not previous_values['build']:
            return

        for data in self.test_env.get('data', []):
            cp(os.path.join(self.test_env['test_dir'], data),
               self.test_env['working_dir'], recursive=True)

        process = check_call(
            self,
            [os.path.join(self.test_env['working_dir'],
                          self.test_env['test_exe'])])
        if '<=== TEST PASSED ===>' not in process.out:
            self.result.set_status(TestStatus.FAIL)
        else:
            self.result.set_status(TestStatus.PASS)
        self.push_result()
