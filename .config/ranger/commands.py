# # This is a sample commands.py.  You can add your own commands here.
# #
# # Please refer to commands_full.py for all the default commands and a complete
# # documentation.  Do NOT add them all here, or you may end up with defunct
# # commands when upgrading ranger.

# # A simple command for demonstration purposes follows.
# # -----------------------------------------------------------------------------

# from __future__ import (absolute_import, division, print_function)

# # You can import any python module as needed.
# import os

# # You always need to import ranger.api.commands here to get the Command class:
# from ranger.api.commands import Command


# # Any class that is a subclass of "Command" will be integrated into ranger as a
# # command.  Try typing ":my_edit<ENTER>" in ranger!
# class my_edit(Command):
#     # The so-called doc-string of the class will be visible in the built-in
#     # help that is accessible by typing "?c" inside ranger.
#     """:my_edit <filename>

#     A sample command for demonstration purposes that opens a file in an editor.
#     """

#     # The execute method is called when you run this command in ranger.
#     def execute(self):
#         # self.arg(1) is the first (space-separated) argument to the function.
#         # This way you can write ":my_edit somefilename<ENTER>".
#         if self.arg(1):
#             # self.rest(1) contains self.arg(1) and everything that follows
#             target_filename = self.rest(1)
#         else:
#             # self.fm is a ranger.core.filemanager.FileManager object and gives
#             # you access to internals of ranger.
#             # self.fm.thisfile is a ranger.container.file.File object and is a
#             # reference to the currently selected file.
#             target_filename = self.fm.thisfile.path

#         # This is a generic function to print text in ranger.
#         self.fm.notify("Let's edit the file " + target_filename + "!")

#         # Using bad=True in fm.notify allows you to print error messages:
#         if not os.path.exists(target_filename):
#             self.fm.notify("The given file does not exist!", bad=True)
#             return

#         # This executes a function from ranger.core.acitons, a module with a
#         # variety of subroutines that can help you construct commands.
#         # Check out the source, or run "pydoc ranger.core.actions" for a list.
#         self.fm.edit_file(target_filename)

#     # The tab method is called when you press tab, and should return a list of
#     # suggestions that the user will tab through.
#     # tabnum is 1 for <TAB> and -1 for <S-TAB> by default
#     def tab(self, tabnum):
#         # This is a generic tab-completion function that iterates through the
#         # content of the current directory.
#         return self._tab_directory_content()

## @https://wiki.archlinux.org/index.php/Ranger

## Archive extraction
## The following command implements archive extraction by copying (yy) one or more archive files and then executing :extracthere on the desired directory.
import os
from ranger.core.loader import CommandLoader

class extracthere(Command):
    def execute(self):
        """ Extract copied files to current directory """
        copied_files = tuple(self.fm.copy_buffer)

        if not copied_files:
            return

        def refresh(_):
            cwd = self.fm.get_directory(original_path)
            cwd.load_content()

        one_file = copied_files[0]
        cwd = self.fm.thisdir
        original_path = cwd.path
        au_flags = ['-X', cwd.path]
        au_flags += self.line.split()[1:]
        au_flags += ['-e']

        self.fm.copy_buffer.clear()
        self.fm.cut_buffer = False
        if len(copied_files) == 1:
            descr = "extracting: " + os.path.basename(one_file.path)
        else:
            descr = "extracting files from: " + os.path.basename(one_file.dirname)
        obj = CommandLoader(args=['aunpack'] + au_flags \
                + [f.path for f in copied_files], descr=descr, read=True)

        obj.signal_bind('after', refresh)
        self.fm.loader.add(obj)


## Compression
## The following command allows users to compress several files on the current directory by marking them and then calling :compress package name. It supports name suggestions by getting the basename of the current directory and appending several possibilities for the extension. You need to have atool installed, otherwise you will see an error message when you create the archive.        
class compress(Command):
    def execute(self):
        """ Compress marked files to current directory """
        cwd = self.fm.thisdir
        marked_files = cwd.get_selection()

        if not marked_files:
            return

        def refresh(_):
            cwd = self.fm.get_directory(original_path)
            cwd.load_content()

        original_path = cwd.path
        parts = self.line.split()
        au_flags = parts[1:]

        descr = "compressing files in: " + os.path.basename(parts[1])
        obj = CommandLoader(args=['apack'] + au_flags + \
                [os.path.relpath(f.path, cwd.path) for f in marked_files], descr=descr, read=True)

        obj.signal_bind('after', refresh)
        self.fm.loader.add(obj)

    def tab(self, tabnum):
        """ Complete with current folder name """

        extension = ['.zip', '.tar.gz', '.rar', '.7z']
        return ['compress ' + os.path.basename(self.fm.thisdir.path) + ext for ext in extension]


## Image mounting
## The following command assumes you are using CDemu as your image mounter and some kind of system like autofs which mounts the virtual drive to a specified location ('/media/virtualrom' in this case). Do not forget to change mountpath to reflect your system settings.
## To mount an image (or images) to a cdemud virtual drive from ranger you select the image files and then type ':mount' on the console. The mounting may actually take some time depending on your setup (in mine it may take as long as one minute) so the command uses a custom loader that waits until the mount directory is mounted and then opens it on the background in tab 9.
import os, time
from ranger.core.loader import Loadable
from ranger.ext.signals import SignalDispatcher
from ranger.ext.shell_escape import *

class MountLoader(Loadable, SignalDispatcher):
    """
    Wait until a directory is mounted
    """
    def __init__(self, path):
        SignalDispatcher.__init__(self)
        descr = "Waiting for dir '" + path + "' to be mounted"
        Loadable.__init__(self, self.generate(), descr)
        self.path = path

    def generate(self):
        available = False
        while not available:
            try:
                if os.path.ismount(self.path):
                    available = True
            except:
                pass
            yield
            time.sleep(0.03)
        self.signal_emit('after')

class mount(Command):
    def execute(self):
        selected_files = self.fm.thisdir.get_selection()

        if not selected_files:
            return

        space = ' '
        self.fm.execute_command("cdemu -b system unload 0")
        self.fm.execute_command("cdemu -b system load 0 " + \
                space.join([shell_escape(f.path) for f in selected_files]))
 
        mountpath = "/mnt/virtual/"

        def mount_finished(path):
            currenttab = self.fm.current_tab
            self.fm.tab_open(9, mountpath)
            self.fm.tab_open(currenttab)

        obj = MountLoader(mountpath)
        obj.signal_bind('after', mount_finished)
        self.fm.loader.add(obj)
