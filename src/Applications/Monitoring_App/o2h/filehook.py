from gmaopy.hooks.datahook import DataHook
from gmaopy.stats.statistics import Statistics
import os

class FileHook(DataHook):

    defaults_ = dict(
        separator = ',',
        hidemissing = False,
        splitdata = True,
        cleanupdata = True,
        formatter = None,
    )

    usage_ = dict(
        separator = "%s: (%s), the character used to separate columns",
        hidemissing = "%s: (%r) if missing values are present, do not display them",
        splitdata = "%s: (%r), separate data to have one value per row",
        cleanupdata = "%s: (%r), convert one element lists to the element, convert list to tuples",
        formatter = "%s: (%s) a callable object which receives a list of strings for each line to be processed, by default it prints out the strings separated by the 'separator'"
    )

    @classmethod
    def usage(self):
        text = []
        text.append("filehook usage:")
        text.append(' ')
        text.append("arg1, arg2, ..., argn: names of the columns to display")
        for k,i in self.defaults_.items():
            text.append('   ' + self.usage_[k] % (k,i))
        text.append(' ')
        text.append("example:")
        text.append("   filehook('date','domain_name','statistic','value',separator='|',hidemissing=True, filename='blah.txt')")
        text.append(' ')
        return text

    def __init__(self,*args,**kwargs):
        self.keywords_ = args
        options = dict(self.defaults_,**kwargs)
        for k,i in options.items():
            setattr(self,k,i)
        if self.formatter is None:
            self.formatter = self.output

    def __call__(self,main_variable,data):
        if os.path.exists(self.filename):
            os.remove(self.filename)
        if self.splitdata:
            data = self.splitOnMainVariable(main_variable,data)
        if self.cleanupdata:
            data = self.cleanupData(data)
        self.formatter(self.keywords_)
        for d in data:
            disp = []
            for key in self.keywords_:
                if not (self.hidemissing and d['value'] == Statistics.missing_value):
                    disp.append(str(d[key]))

            if len(disp) > 0:
                self.formatter(disp)
#        print

    def output(self,line):
        #print self.separator.join(line)
        if os.path.exists(self.filename):
            with open(self.filename, 'a') as f:
                f.write(self.separator.join(line)+'\n')
        else:
            with open(self.filename, 'w') as f:
                f.write(self.separator.join(line)+'\n')

filehook = FileHook

DataHook.registerHook('filehook',FileHook)
