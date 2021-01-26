# -*- coding: utf-8 -*-
# This file is part of ranger, the console file manager.
# This configuration file is licensed under the same terms as ranger.
# ===================================================================
#
# NOTE: If you copied this file to /etc/ranger/commands_full.py or
# ~/.config/ranger/commands_full.py, then it will NOT be loaded by ranger,
# and only serve as a reference.
#
# ===================================================================
# This file contains ranger's commands.
# It's all in python; lines beginning with # are comments.
#
# Note that additional commands are automatically generated from the methods
# of the class ranger.core.actions.Actions.
#
# You can customize commands in the files /etc/ranger/commands.py (system-wide)
# and ~/.config/ranger/commands.py (per user).
# They have the same syntax as this file.  In fact, you can just copy this
# file to ~/.config/ranger/commands_full.py with
# `ranger --copy-config=commands_full' and make your modifications, don't
# forget to rename it to commands.py.  You can also use
# `ranger --copy-config=commands' to copy a short sample commands.py that
# has everything you need to get started.
# But make sure you update your configs when you update ranger.
#
# ===================================================================
# Every class defined here which is a subclass of `Command' will be used as a
# command in ranger.  Several methods are defined to interface with ranger:
#   execute():   called when the command is executed.
#   cancel():    called when closing the console.
#   tab(tabnum): called when <TAB> is pressed.
#   quick():     called after each keypress.
#
# tab() argument tabnum is 1 for <TAB> and -1 for <S-TAB> by default
#
# The return values for tab() can be either:
#   None: There is no tab completion
#   A string: Change the console to this string
#   A list/tuple/generator: cycle through every item in it
#
# The return value for quick() can be:
#   False: Nothing happens
#   True: Execute the command afterwards
#
# The return value for execute() and cancel() doesn't matter.
#
# ===================================================================
# Commands have certain attributes and methods that facilitate parsing of
# the arguments:
#
# self.line: The whole line that was written in the console.
# self.args: A list of all (space-separated) arguments to the command.
# self.quantifier: If this command was mapped to the key "X" and
#      the user pressed 6X, self.quantifier will be 6.
# self.arg(n): The n-th argument, or an empty string if it doesn't exist.
# self.rest(n): The n-th argument plus everything that followed.  For example,
#      if the command was "search foo bar a b c", rest(2) will be "bar a b c"
# self.start(n): Anything before the n-th argument.  For example, if the
#      command was "search foo bar a b c", start(2) will be "search foo"
#
# ===================================================================
# And this is a little reference for common ranger functions and objects:
#
# self.fm: A reference to the "fm" object which contains most information
#      about ranger.
# self.fm.notify(string): Print the given string on the screen.
# self.fm.notify(string, bad=True): Print the given string in RED.
# self.fm.reload_cwd(): Reload the current working directory.
# self.fm.thisdir: The current working directory. (A File object.)
# self.fm.thisfile: The current file. (A File object too.)
# self.fm.thistab.get_selection(): A list of all selected files.
# self.fm.execute_console(string): Execute the string as a ranger command.
# self.fm.open_console(string): Open the console with the given string
#      already typed in for you.
# self.fm.move(direction): Moves the cursor in the given direction, which
#      can be something like down=3, up=5, right=1, left=1, to=6, ...
#
# File objects (for example self.fm.thisfile) have these useful attributes and
# methods:
#
# tfile.path: The path to the file.
# tfile.basename: The base name only.
# tfile.load_content(): Force a loading of the directories content (which
#      obviously works with directories only)
# tfile.is_directory: True/False depending on whether it's a directory.
#
# For advanced commands it is unavoidable to dive a bit into the source code
# of ranger.
# ===================================================================

from __future__ import (absolute_import, division, print_function)

from collections import deque
import os
import re

from ranger.api.commands import Command


class alias(Command):
    """:alias <newcommand> <oldcommand>
    Copies the oldcommand as newcommand.
    """

    context = 'browser'
    resolve_macros = False

    def execute(self):
        if not self.arg(1) or not self.arg(2):
            self.fm.notify('Syntax: alias <newcommand> <oldcommand>', bad=True)
            return

        self.fm.commands.alias(self.arg(1), self.rest(2))


class bulkrename(Command):
    """:bulkrename
    This command opens a list of selected files in an external editor. EMACS
    After you edit and save the file, it will generate a shell script
    which does bulk renaming according to the changes you did in the file.
    This shell script is opened in an editor for you to review.
    After you close it, it will be executed.
    """

    def execute(self):
        # pylint: disable=too-many-locals,too-many-statements,too-many-branches
        import sys
        import tempfile
        from ranger.container.file import File
        from ranger.ext.shell_escape import shell_escape as esc
        py3 = sys.version_info[0] >= 3

        # Create and edit the file list
        filenames = [f.relative_path for f in self.fm.thistab.get_selection()]
        with tempfile.NamedTemporaryFile(delete=False) as listfile:
            listpath = listfile.name
            if py3:
                listfile.write("\n".join(filenames).encode(
                    encoding="utf-8", errors="surrogateescape"))
            else:
                listfile.write("\n".join(filenames))
        self.fm.execute_file([File(listpath)], app='emacs')
        with (open(listpath, 'r', encoding="utf-8", errors="surrogateescape") if
              py3 else open(listpath, 'r')) as listfile:
            new_filenames = listfile.read().split("\n")
        os.unlink(listpath)
        if all(a == b for a, b in zip(filenames, new_filenames)):
            self.fm.notify("No renaming to be done!")
            return

        # Generate script
        with tempfile.NamedTemporaryFile() as cmdfile:
            script_lines = []
            script_lines.append("# This file will be executed when you close"
                                " the editor.")
            script_lines.append("# Please double-check everything, clear the"
                                " file to abort.")
            new_dirs = []
            for old, new in zip(filenames, new_filenames):
                if old != new:
                    basepath, _ = os.path.split(new)
                    if (basepath and basepath not in new_dirs
                            and not os.path.isdir(basepath)):
                        script_lines.append("mkdir -vp -- {dir}".format(
                            dir=esc(basepath)))
                        new_dirs.append(basepath)
                    script_lines.append("mv -vi -- {old} {new}".format(
                        old=esc(old), new=esc(new)))
            # Make sure not to forget the ending newline
            script_content = "\n".join(script_lines) + "\n"
            if py3:
                cmdfile.write(script_content.encode(encoding="utf-8",
                                                    errors="surrogateescape"))
            else:
                cmdfile.write(script_content)
            cmdfile.flush()

            # Open the script and let the user review it, then check if the
            # script was modified by the user
            self.fm.execute_file([File(cmdfile.name)], app='editor')
            cmdfile.seek(0)
            script_was_edited = (script_content != cmdfile.read())

            # Do the renaming
            self.fm.run(['/bin/sh', cmdfile.name], flags='w')

        # Retag the files, but only if the script wasn't changed during review,
        # because only then we know which are the source and destination files.
        if not script_was_edited:
            tags_changed = False
            for old, new in zip(filenames, new_filenames):
                if old != new:
                    oldpath = self.fm.thisdir.path + '/' + old
                    newpath = self.fm.thisdir.path + '/' + new
                    if oldpath in self.fm.tags:
                        old_tag = self.fm.tags.tags[oldpath]
                        self.fm.tags.remove(oldpath)
                        self.fm.tags.tags[newpath] = old_tag
                        tags_changed = True
            if tags_changed:
                self.fm.tags.dump()
        else:
            fm.notify("files have not been retagged")

#  ___   ___         __                     ___         ___         ___        __  
#  `._|=|   |   .'|=|  |   .'|=|`.     .'| |   |   .'|=|_.'    .'|=|_.'   .'|=|  | 
#       |  .' .'  | |  | .'  | |  `. .'  |\|   | .'  |___    .'  |  ___ .'  | |  | 
#   .'|=|.'   |   |=|.'  |   |=|   | |   | |   | |   |`._|=. |   |=|_.' |   |=|.'  
# .'  |  ___  |   |  |`. |   | |   | |   | |  .' `.  |  __|| |   |  ___ |   |  |`. 
# |___|=|_.'  |___|  |_| |___| |___| |___| |.'     `.|=|_.'' |___|=|_.' |___|  |_| 
#
## https://github.com/Vifon/zranger

import os
import signal
def zranger_chdir_handler(signal, frame):
    tmpfile = "/tmp/zranger-cwd-{}".format(os.getuid())
    with open(tmpfile, "r") as f:
        Command.fm.cd(f.readline().strip())
        os.unlink(tmpfile)
signal.signal(signal.SIGUSR1, zranger_chdir_handler)

class tmux_detach(Command):
    """
    :tmux_detach

    Detach from this tmux session (if inside tmux).
    """
    def execute(self):
        if not os.environ.get('TMUX'):
            return
        os.system("tmux detach")
