-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published
--   by the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

-- Revision History
--   18.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Interfaces.C;
with Interfaces;

package OpenGL is

   OpenGLError : Exception;

   -- Portability : Maybe risky and non portable when using anything but
   --               the GNAT compiler
   type GLdouble_Type is new Long_Float;
   type GLclampf_Type is new Float;

   type GLbitfield_Type is new Interfaces.C.unsigned;
   type GLint_Type is new Interfaces.C.int;
   type GLint_Access is access all GLint_Type;
   type GLsizei_Type is new Interfaces.C.int;
   type GLsizei_Access is access all GLsizei_Type;
   type GLenum_Type is new Interfaces.Unsigned_32;

   GL_MODELVIEW  : constant GLenum_Type:=16#1700#;
   GL_PROJECTION : constant GLenum_Type:=16#1701#;

   GL_SCISSOR_TEST : constant GLenum_Type:=16#C11#;
   GL_DEPTH_TEST   : constant GLenum_Type:=16#B71#;
   GL_BLEND        : constant GLenum_Type:=16#BE2#;
   GL_TEXTURE_2D   : constant GLenum_Type:=16#DE1#;

   GL_SRC_ALPHA           : constant GLenum_Type:=16#302#;
   GL_ONE_MINUS_SRC_ALPHA : constant GLenum_Type:=16#303#;
   GL_GREATER             : constant GLenum_Type:=16#204#;

   GL_COLOR_BUFFER_BIT : constant GLbitfield_Type:=16#4000#;
   GL_DEPTH_BUFFER_BIT : constant GLbitfield_Type:=16#100#;


   procedure glFinish;
   pragma Import(StdCall,glFinish,"glFinish");

   procedure glViewport
     (x      : GLint_Type;
      y      : GLint_Type;
      width  : GLsizei_Type;
      height : GLsizei_Type);
   pragma Import(StdCall,glViewport,"glViewport");

   procedure glMatrixMode
     (mode : GLenum_Type);
   pragma Import(StdCall,glMatrixMode,"glMatrixMode");

   procedure glLoadIdentity;
   pragma Import(StdCall,glLoadIdentity,"glLoadIdentity");

   procedure glOrtho
     (left    : GLdouble_Type;
      right   : GLdouble_Type;
      bottom  : GLdouble_Type;
      top     : GLdouble_Type;
      nearVal : GLdouble_Type;
      farVal  : GLdouble_Type);
   pragma Import(StdCall,glOrtho,"glOrtho");

   procedure glClearColor
     (red   : GLclampf_Type;
      green : GLclampf_Type;
      blue  : GLclampf_Type;
      alpha : GLclampf_Type);
   pragma Import(StdCall,glClearColor,"glClearColor");

   procedure glClear
     (mask : GLbitfield_Type);
   pragma Import(StdCall,glClear,"glClear");

   procedure glEnable
     (cap : GLenum_Type);
   pragma Import(StdCall,glEnable,"glEnable");

   procedure glDisable
     (cap : GLenum_Type);
   pragma Import(StdCall,glDisable,"glDisable");

   function glGetError
     return GLenum_Type;
   pragma Import(StdCall,glGetError,"glGetError");

   procedure glBlendFunc
     (sfactor : GLenum_Type;
      dfactor : GLenum_Type);
   pragma Import(StdCall,glBlendFUnc,"glBlendFunc");

   procedure glAlphaFunc
     (func : GLenum_Type;
      ref  : GLclampf_Type);
   pragma Import(StdCall,glAlphaFunc,"glAlphaFunc");

   procedure AssertError;

end OpenGL;
