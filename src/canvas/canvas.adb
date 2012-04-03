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

pragma Ada_2005;

package body Canvas is

   procedure GetPixel
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Color  : out Color_Type) is
   begin
      if (X in Canvas.Image'Range(1))
        and (Y in Canvas.Image'Range(2)) then
         Color:=Canvas.Image(X,Y);
      else
         Color:=0;
      end if;
   end;
   ---------------------------------------------------------------------------

   procedure SetPixel
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Color  : Color_Type) is
   begin

      if (X in Canvas.Image'Range(1))
        and (Y in Canvas.Image'Range(2)) then
         Canvas.Image(X,Y):=Color;
      end if;

   end SetPixel;
   ---------------------------------------------------------------------------

   procedure Bar
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Height : Integer;
      Width  : Integer;
      Color  : Color_Type) is

      DrawHeight : Integer;
      DrawWidth  : Integer;
      DrawX      : Integer;
      DrawY       : Integer;

   begin
      DrawHeight := Height;
      DrawY      := Y;

      if DrawY<0 then
         DrawHeight:=DrawHeight+DrawY;
         DrawY:=0;
      end if;

      if DrawY+DrawHeight>Canvas.ContentHeight then
         DrawHeight:=Canvas.ContentHeight-DrawY;
      end if;

      if DrawHeight<=0 then
         return;
      end if;
      ------------------------------------------------------------------------

      DrawWidth := Width;
      DrawX     := X;

      if DrawX<0 then
         DrawWidth:=DrawWidth+DrawX;
         DrawX:=0;
      end if;

      if DrawX+DrawWidth>Canvas.ContentWidth then
         DrawWidth:=Canvas.ContentWidth-DrawX;
      end if;

      if DrawWidth<=0 then
         return;
      end if;
      ------------------------------------------------------------------------

      for i in DrawY..DrawY+DrawHeight-1 loop
         for n in DrawX..DrawX+DrawWidth-1 loop
            Canvas.Image(i,n):=Color;
         end loop;
      end loop;
   end Bar;
   ---------------------------------------------------------------------------

   procedure HorzLine
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Color  : Color_Type) is

      DrawWidth : Integer;
      DrawX     : Integer;

   begin
      if (Y<0)
        or (Y>=Canvas.ContentHeight) then
         return;
      end if;

      DrawWidth := Width;
      DrawX     := X;

      if DrawX<0 then
         DrawWidth:=DrawWidth+DrawX;
         DrawX:=0;
      end if;

      if DrawX+DrawWidth>Canvas.ContentWidth then
         DrawWidth:=Canvas.ContentWidth-DrawX;
      end if;

      if DrawWidth<=0 then
         return;
      end if;

      for i in DrawX..DrawX+DrawWidth-1 loop
         Canvas.Image(Y,i):=Color;
      end loop;

   end HorzLine;
   ---------------------------------------------------------------------------

   procedure VertLine
     (Canvas : in out Canvas_Type;
      X      : Integer;
      Y      : Integer;
      Height : Integer;
      Color  : Color_Type) is

      DrawHeight : Integer;
      DrawY      : Integer;

   begin
      if (X<0)
        or (X>=Canvas.ContentWidth) then
         return;
      end if;

      DrawHeight := Height;
      DrawY      := Y;

      if DrawY<0 then
         DrawHeight:=DrawHeight+DrawY;
         DrawY:=0;
      end if;

      if DrawY+DrawHeight>Canvas.ContentHeight then
         DrawHeight:=Canvas.ContentHeight-DrawY;
      end if;

      if DrawHeight<=0 then
         return;
      end if;

      for i in DrawY..DrawY+DrawHeight-1 loop
         Canvas.Image(i,X):=Color;
      end loop;

   end VertLine;

   procedure Clear
     (Canvas : in out Canvas_Type;
      Color  : Color_Type) is
   begin
      for y in Canvas.Image'Range(1) loop
         for x in Canvas.Image'Range(2) loop
            Canvas.Image(y,x):=Color;
         end loop;
      end loop;
      Canvas.Modified := True;
   end;
   ---------------------------------------------------------------------------
   function MultiplyAlpha
     (Color : Color_Type;
      Alpha : Integer)
      return Color_Type is

   begin
      return Color and 16#FFFFFF#+
        Shift_Left(Shift_Right(Color,24)*Color_Type(Alpha)/255,24);

   end MultiplyAlpha;
   ---------------------------------------------------------------------------

   function PreBlendMix
     (BackgroundColor : Color_Type;
      ForegroundColor      : Color_Type)
      return Color_Type is

      use Interfaces;

      Alpha1    : constant Color_Type:=Shift_Right(BackgroundColor,24);
      Alpha2    : constant Color_Type:=Shift_Right(ForegroundColor,24);
      Red1      : constant Color_Type:=Shift_Right(BackgroundColor,16) and 16#FF#;
      Red2      : constant Color_Type:=Shift_Right(ForegroundColor,16) and 16#FF#;
      Green1    : constant Color_Type:=Shift_Right(BackgroundColor,8) and 16#FF#;
      Green2    : constant Color_Type:=Shift_Right(ForegroundColor,8) and 16#FF#;
      Blue1     : constant Color_Type:=BackgroundColor and 16#FF#;
      Blue2     : constant Color_Type:=ForegroundColor and 16#FF#;

      NormAlpha : Color_Type;
      NewRed    : Color_Type;
      NewGreen  : Color_Type;
      NewBlue   : Color_Type;
      NewAlpha  : Color_Type;

   begin

      NormAlpha := (255*255-(255-Alpha1)*(255-Alpha2));

      if NormAlpha/=0 then
         NewRed   := (255*Alpha2*Red2+(255-Alpha2)*Alpha1*Red1)/NormAlpha;
         NewGreen := (255*Alpha2*Green2+(255-Alpha2)*Alpha1*Green1)/NormAlpha;
         NewBlue  := (255*Alpha2*Blue2+(255-Alpha2)*Alpha1*Blue1)/NormAlpha;
         NewAlpha := NormAlpha/255;
         return NewRed
           +Shift_Left(NewGreen,8)
           +Shift_Left(NewBlue,16)
           +Shift_Left(NewAlpha,24);
      else
         return 0;
      end if;

   end;

end Canvas;
