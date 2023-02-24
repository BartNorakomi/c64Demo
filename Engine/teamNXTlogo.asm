TeamNXTLogoRoutine:

  call  SetInterruptHandlerC64Demo

  ld    a,3
  ld    (scrollspeed),a

WorldLoader:
;  call  ScreenOff

;  ld    hl,CleanPage2
;  call  DoCopy
  call  LoadWorld
  
;  ld    a,0*32+31
;  call  setpage                   ;in a->x*32+31 (x=page)
    
  .loop:
  xor   a
  ld    hl,vblankintflag
.checkflag:
  cp    (hl)
  jr    z,.checkflag
  ld    (hl),a  

  call  .HandlePhase
  ld    a,(framecounter)
  inc   a
  ld    (framecounter),a
  
  call  PopulateControls
  call  HandleScrollSpeed
;
; bit	7	6	  5		    4		    3		    2		  1		  0
;		  0	0	  trig-b	trig-a	right	  left	down	up	(joystick)
;		  0	F1	'M'		  space	  right	  left	down	up	(keyboard)
;
  ld    a,(NewPrContr)	
  bit   4,a               ;check  if space is pressed
  jr    z,.loop
  jp    WorldLoader

.HandlePhase:
  ld    a,(scrollspeed)
  or    a
  ld    b,0
  jr    z,.ScrollSpeedSet
  dec   a
  ld    b,1
  jr    z,.ScrollSpeedSet
  dec   a
  ld    b,3
  jr    z,.ScrollSpeedSet
  dec   a
  ld    b,7
  jr    z,.ScrollSpeedSet
  dec   a
  ld    b,15
  jr    z,.ScrollSpeedSet
  ld    b,31

  .ScrollSpeedSet:
  ld    a,(framecounter)
  and   7
  jr    z,.IncreaseScrollCounter

  ld    a,(scrollcounter)
  or    a
  jp    .CheckStep

  .IncreaseScrollCounter:
  ld    a,(scrollcounter)
  inc   a
  and   15
  ld    (scrollcounter),a
  .CheckStep:
   
  jp    z,.step0
  dec   a
  jp    z,.step1
  dec   a
  jp    z,.step2
  dec   a
  jp    z,.step3
  dec   a
  jp    z,.step4
  dec   a
  jp    z,.step5
  dec   a
  jp    z,.step6
  dec   a
  jp    z,.step7
  dec   a
  jp    z,.step8
  dec   a
  jp    z,.step9
  dec   a
  jp    z,.step10
  dec   a
  jp    z,.step11
  dec   a
  jp    z,.step12
  dec   a
  jp    z,.step13
  dec   a
  jp    z,.step14






.step15:
  ld    a,lineintheightC64Demo    ;r#23 set on vblank
  ld    b,lineintheightC64Demo+lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,+lineintheightC64Demo-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteB)
  call  setpalette

  ld    a,3*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret

.step14:
  xor   a                         ;r#23 set on vblank
  ld    b,lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteB)
  call  setpalette

  ld    a,3*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret

.step13:
  ld    a,lineintheightC64Demo    ;r#23 set on vblank
  ld    b,lineintheightC64Demo+lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,+lineintheightC64Demo-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteB)
  call  setpalette

  ld    a,2*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret

.step12:
  xor   a                         ;r#23 set on vblank
  ld    b,lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteB)
  call  setpalette

  ld    a,2*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret

.step11:
  ld    a,lineintheightC64Demo    ;r#23 set on vblank
  ld    b,lineintheightC64Demo+lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,+lineintheightC64Demo-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteB)
  call  setpalette

  ld    a,1*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret

.step10:
  xor   a                         ;r#23 set on vblank
  ld    b,lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteB)
  call  setpalette

  ld    a,1*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret

.step9:
  ld    a,lineintheightC64Demo    ;r#23 set on vblank
  ld    b,lineintheightC64Demo+lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,+lineintheightC64Demo-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteB)
  call  setpalette

  ld    a,0*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret

.step8:
  xor   a                         ;r#23 set on vblank
  ld    b,lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteB)
  call  setpalette

  ld    a,0*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret

















.step7:
  ld    a,lineintheightC64Demo    ;r#23 set on vblank
  ld    b,lineintheightC64Demo+lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,+lineintheightC64Demo-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteA)
  call  setpalette

  ld    a,3*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret

.step6:
  xor   a                         ;r#23 set on vblank
  ld    b,lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteA)
  call  setpalette

  ld    a,3*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret

.step5:
  ld    a,lineintheightC64Demo    ;r#23 set on vblank
  ld    b,lineintheightC64Demo+lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,+lineintheightC64Demo-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteA)
  call  setpalette

  ld    a,2*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret

.step4:
  xor   a                         ;r#23 set on vblank
  ld    b,lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteA)
  call  setpalette

  ld    a,2*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret

.step3:
  ld    a,lineintheightC64Demo    ;r#23 set on vblank
  ld    b,lineintheightC64Demo+lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,+lineintheightC64Demo-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteA)
  call  setpalette

  ld    a,1*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret

.step2:
  xor   a                         ;r#23 set on vblank
  ld    b,lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteA)
  call  setpalette

  ld    a,1*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret

.step1:
  ld    a,lineintheightC64Demo    ;r#23 set on vblank
  ld    b,lineintheightC64Demo+lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,+lineintheightC64Demo-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteA)
  call  setpalette

  ld    a,0*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret

.step0:
  xor   a                         ;r#23 set on vblank
  ld    b,lineintheightC64Demo-2    ;r#19 set on vblank
  ld    c,-lineintheightC64Demo   ;lineintheightC64Demo+00 ;r#23 set on lineint
  call  setR19and23onVblankAndLineint

  ld    hl,(worldpaletteA)
  call  setpalette

  ld    a,0*32+31
  call  setpage                   ;in a->x*32+31 (x=page)
  ret






HandleScrollSpeed:
  ld    a,(framecounter)
  and   31
  ret   nz

;
; bit	7	6	  5		    4		    3		    2		  1		  0
;		  0	0	  trig-b	trig-a	right	  left	down	up	(joystick)
;		  0	F1	'M'		  space	  right	  left	down	up	(keyboard)
;
  ld    a,(Controls)
  bit   2,a               ;check  if left is pressed
  jr    nz,.left
  bit   3,a               ;check  if right is pressed
  jr    nz,.right
  ret

  .left:
  ld    a,(scrollspeed)
  cp    5
  ret   z
  inc   a
  ld    (scrollspeed),a
  ret

  .right:
  ld    a,(scrollspeed)
  or    a
  ret   z
  dec   a
  ld    (scrollspeed),a
  ret

lineintheightC64Demo: equ 106
SetInterruptHandlerC64Demo:
  di
  ld    hl,InterruptHandlerC64Demo
  ld    ($38+1),hl          ;set new normal interrupt
  ld    a,$c3               ;jump command
  ld    ($38),a
 
  ld    a,(VDP_0)           ;set ei1
  or    16                  ;ei1 checks for lineint and vblankint
  ld    (VDP_0),a           ;ei0 (which is default at boot) only checks vblankint
  out   ($99),a
  ld    a,128
  out   ($99),a

  ld    a,lineintheightC64Demo
  out   ($99),a
  ld    a,19+128            ;set lineinterrupt height
  ei
  out   ($99),a 
  ret

InterruptHandlerC64Demo:
  push  af
  
  ld    a,1                 ;set s#1
  out   ($99),a
  ld    a,15+128
  out   ($99),a
  in    a,($99)             ;check and acknowledge line interrupt
  rrca
  jp    c,lineintC64Demo    ;ScoreboardSplit/BorderMaskingSplit
  
  xor   a                   ;set s#0
  out   ($99),a
  ld    a,15+128
  out   ($99),a
  in    a,($99)             ;check and acknowledge vblank interrupt
  rlca
  jp    c,vblankC64Demo     ;vblank detected, so jp to that routine
 
  pop   af 
  ei
  ret

;we set horizontal and vertical screen adjust
;we set status register 0
vblankC64Demo:
  ld    a,1
  ld    (vblankintflag),a

  pop   af 
  ei
  ret
  
lineintC64Demo:
  push  bc
  push  de

  ld    c,$99
  ld    a,(R23onLineint)
  ld    d,a
  ld    e,23+128
 
nop |nop |nop |nop |nop |nop 

  out   (c),d             ;set r#23 at the start of Hblank period
  out   (c),e

  xor   a                 ;set s#0
  out   ($99),a
  ld    a,15+128
  out   ($99),a
  
  pop   de 
  pop   bc
  pop   af 
  ei
  ret  

setR19and23onVblankAndLineint:    ;in a=r#23 set on vblank, b=r#19 set on vblank, c=r#23 set on lineint
;  xor   a                         ;r#23 set on vblank
;  ld    b,lineintheightC64Demo+00 ;r#19 set on vblank
;  ld    c,lineintheightC64Demo+00 ;r#23 set on lineint

  di
  out   ($99),a    
  ld    a,23+128
  out   ($99),a

  ld    a,b                         ;splitline height relative to r#23
  out   ($99),a
  ld    a,19+128
  ei
  out   ($99),a  
  
  ld    a,c
  ld    (R23onLineint),a
  ret

World1aPalette:
  incbin "..\grapx\world1\world1a.pl" ;file palette 
World1bPalette:
  incbin "..\grapx\world1\world1b.pl" ;file palette 
World2aPalette:
  incbin "..\grapx\world2\world2a.pl" ;file palette 
World2bPalette:
  incbin "..\grapx\world2\world2b.pl" ;file palette 
World3aPalette:
  incbin "..\grapx\world3\world3a.pl" ;file palette 
World3bPalette:
  incbin "..\grapx\world3\world3b.pl" ;file palette 
World4aPalette:
  incbin "..\grapx\world4\world4a.pl" ;file palette 
World4bPalette:
  incbin "..\grapx\world4\world4b.pl" ;file palette 

LoadWorld:  
  ld    a,(CurrentWorld)
  inc   a
  and   3
  ld    (CurrentWorld),a
  jp    z,LoadWorld4
  dec   a
  jp    z,LoadWorld1
  dec   a
  jp    z,LoadWorld2
  dec   a
  jp    z,LoadWorld3
  ret


LoadWorld1:  
  ld    d,World1Page0GraphicsBlock
  ld    hl,$0000                  ;page 0 - screen 5
  ld    b,0
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    d,World1Page1GraphicsBlock
  ld    hl,$8000                  ;page 1 - screen 5
  ld    b,0
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    d,World1Page2GraphicsBlock
  ld    hl,$0000                  ;page 2 - screen 5
  ld    b,1
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    d,World1Page3GraphicsBlock
  ld    hl,$8000                  ;page 3 - screen 5
  ld    b,1
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    hl,World1aPalette
  ld    (worldpaletteA),hl
  ld    hl,World1bPalette
  ld    (worldpaletteB),hl
  ret

LoadWorld2:  
  ld    d,World2Page0GraphicsBlock
  ld    hl,$0000                  ;page 0 - screen 5
  ld    b,0
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    d,World2Page1GraphicsBlock
  ld    hl,$8000                  ;page 1 - screen 5
  ld    b,0
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    d,World2Page2GraphicsBlock
  ld    hl,$0000                  ;page 2 - screen 5
  ld    b,1
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    d,World2Page3GraphicsBlock
  ld    hl,$8000                  ;page 3 - screen 5
  ld    b,1
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    hl,World2aPalette
  ld    (worldpaletteA),hl
  ld    hl,World2bPalette
  ld    (worldpaletteB),hl
  ret

LoadWorld3:  
  ld    d,World3Page0GraphicsBlock
  ld    hl,$0000                  ;page 0 - screen 5
  ld    b,0
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    d,World3Page1GraphicsBlock
  ld    hl,$8000                  ;page 1 - screen 5
  ld    b,0
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    d,World3Page2GraphicsBlock
  ld    hl,$0000                  ;page 2 - screen 5
  ld    b,1
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    d,World3Page3GraphicsBlock
  ld    hl,$8000                  ;page 3 - screen 5
  ld    b,1
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    hl,World3aPalette
  ld    (worldpaletteA),hl
  ld    hl,World3bPalette
  ld    (worldpaletteB),hl
  ret

LoadWorld4:  
  ld    d,World4Page0GraphicsBlock
  ld    hl,$0000                  ;page 0 - screen 5
  ld    b,0
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    d,World4Page1GraphicsBlock
  ld    hl,$8000                  ;page 1 - screen 5
  ld    b,0
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    d,World4Page2GraphicsBlock
  ld    hl,$0000                  ;page 2 - screen 5
  ld    b,1
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    d,World4Page3GraphicsBlock
  ld    hl,$8000                  ;page 3 - screen 5
  ld    b,1
  call  copyGraphicsToScreen      ;copies $8000 bytes (256x256) to screen

  ld    hl,World4aPalette
  ld    (worldpaletteA),hl
  ld    hl,World4bPalette
  ld    (worldpaletteB),hl
  ret

LogoAnimationPhase6:              ;6 = fade out frame
  ld    a,(LogoAnimationVar3)
  inc   a
  ld    (LogoAnimationVar3),a
  
  cp    09 + 80
  ld    hl,framestepdarker1Palette
  jr    z,.GoSet4PaletteColors
  cp    18 + 80
  ld    hl,framestepdarker2Palette
  jr    z,.GoSet4PaletteColors
  cp    27 + 80
  ld    hl,framestepdarker3Palette
  jr    z,.GoSet4PaletteColors
  cp    36 + 80
  ld    hl,framestepdarker4Palette
  jr    z,.GoSet4PaletteColors
  cp    45 + 80
  ld    hl,framestepdarker5Palette
  jr    z,.GoSet4PaletteColors
  cp    100 + 80
  ret   nz
  pop   af
  ret
  
  .GoSet4PaletteColors:
  
  xor   a                         ;start write to this palette color (0)
	out		($99),a
	ld		a,16+128
	out		($99),a
  
  call  Set4PaletteColors
  ret

WhichBlocksTable1: ;from transparant to non transparant
  db    1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,1,0,1,1,0,1,1,1,0,1,1,1,1,0,1,1,1,1,1,0
WhichBlocksTable2: ;non transparant to black
  db    1,1,2,1,1,1,2,1,1,1,1,1,2,1,1,1,1,2,1,1,1,2,1,1,2,1,2,2,1,2,2,2,1,2,2,2,2,1,2,2,2,2,2,1,2

LogoAnimationPhase5:              ;6 = fade out from white to normal palette
  ld    a,(LogoAnimationVar3)     ;wait x frames
  dec   a
  jr    z,.Go
  ld    (LogoAnimationVar3),a     ;wait x frames
  ret
  .Go:

  ld    a,(framecounter)
  and   3
  ret   nz

  ld    hl,TeamNXTLogoPalette

  xor   a                         ;start write to this palette color (0)
	out		($99),a
	ld		a,16+128
	out		($99),a
  
  ld    b,16                      ;16 colors
  ld    a,(LogoAnimationVar1)     ;palette step  
  ld    c,a
  dec   a
  ld    (LogoAnimationVar1),a     ;palette step  
  jp    p,.NotEnd
  ld    a,6
  ld    (LogoAnimationPhase),a  
  xor   a
  ld    (LogoAnimationVar3),a     ;palette step  
  jp    setpalette

  .NotEnd:
  ld    a,(LogoAnimationVar2)     ;palette step  
  ld    d,a
  sub   %0001 0001
  ld    (LogoAnimationVar2),a     ;palette step  
  .loop:
  ld    a,(hl)                    ;0 R2 R1 R0 0 B2 B1 B0
  add   a,d
  bit   3,a
  jr    z,.EndCheckOverflowB
  or    %0000 0111
  .EndCheckOverflowB:
  bit   7,a
  jr    z,.EndCheckOverflowR
  or    %0111 0000
  .EndCheckOverflowR:
  out   ($9a),a                   ;red + blue
  inc   hl
  ld    a,(hl)                    ;0 0 0 0 0 G2 G1 G0
  add   a,c
  bit   3,a
  jr    z,.EndCheckOverflowG
  ld    a,%0000 0111
  .EndCheckOverflowG:
  out   ($9a),a                   ;green
  inc   hl
  
  djnz  .loop  
  ret

Set4PaletteColors:
	xor		a
	di
	out		($99),a
	ld		a,16+128
	out		($99),a
	ld		bc,$089A
	otir
	ei
	ret

framestep1Palette:
  incbin "..\grapx\TeamNXTLogo\framestep1.PL",0,8 ;file palette 
framestep2Palette:
  incbin "..\grapx\TeamNXTLogo\framestep2.PL",0,8 ;file palette 
framestep3Palette:
  incbin "..\grapx\TeamNXTLogo\framestep3.PL",0,8 ;file palette 
framestep4Palette:
  incbin "..\grapx\TeamNXTLogo\framestep4.PL",0,8 ;file palette 
framestep5Palette:
  incbin "..\grapx\TeamNXTLogo\framestep5.PL",0,8 ;file palette 
framestep6Palette:
  incbin "..\grapx\TeamNXTLogo\framestep6.PL",0,8 ;file palette 

framestepdarker1Palette:
  incbin "..\grapx\TeamNXTLogo\framestepdarker1.PL",0,8 ;file palette 
framestepdarker2Palette:
  incbin "..\grapx\TeamNXTLogo\framestepdarker2.PL",0,8 ;file palette 
framestepdarker3Palette:
  incbin "..\grapx\TeamNXTLogo\framestepdarker3.PL",0,8 ;file palette 
framestepdarker4Palette:
  incbin "..\grapx\TeamNXTLogo\framestepdarker4.PL",0,8 ;file palette 
framestepdarker5Palette:
  incbin "..\grapx\TeamNXTLogo\framestepdarker5.PL",0,8 ;file palette 

fadeinLogoStep0:
  incbin "..\grapx\TeamNXTLogo\fadeinlogostep0.PL"  ;file palette 
fadeinLogoStep1:
  incbin "..\grapx\TeamNXTLogo\fadeinlogostep1.PL"  ;file palette 
fadeinLogoStep2:
  incbin "..\grapx\TeamNXTLogo\fadeinlogostep2.PL"  ;file palette 
fadeinLogoStep3:
  incbin "..\grapx\TeamNXTLogo\fadeinlogostep3.PL"  ;file palette 
fadeinLogoStep4:
  incbin "..\grapx\TeamNXTLogo\fadeinlogostep4.PL"  ;file palette 
fadeinLogoStep5:
  incbin "..\grapx\TeamNXTLogo\fadeinlogostep5.PL"  ;file palette 
TeamNXTLogoBlackFramePalette:
  incbin "..\grapx\TeamNXTLogo\TeamNXTLogoBlackFramePalette.PL"  ;file palette 

FirstFourColorsWhite: db  %0111 0111, %0000 0111, %0111 0111, %0000 0111, %0111 0111, %0000 0111, %0111 0111, %0000 0111
LogoAnimationPhase4:              ;4 = put final logo
  ld    a,2*32+31                 ;back to page 2
  call  setpage                   ;in a->x*32+31 (x=page)

  ld    a,1
  ld    (CopyLogoPart+sPage),a
  ld    a,0
  call  CopyLogoPartRoutine       ;total logo non transparant
  ld    a,1
  call  CopyLogoPartRoutine

  ld    a,5
  ld    (LogoAnimationPhase),a
  ld    a,7
  ld    (LogoAnimationVar1),a     ;palette step
  ld    a,7*16 + 7
  ld    (LogoAnimationVar2),a     ;palette step
  ld    a,5
  ld    (LogoAnimationVar3),a     ;wait x frames
  ret

LogoAnimationPhase3:              ;fade screen to white
  call  SmallBlock                ;keep rotating blocks during fade
  call  BigBlock

  ld    a,(framecounter)
  and   1
  ret   nz

  ld    hl,TeamNXTLogoPalette

  xor   a                         ;start write to this palette color (0)
	out		($99),a
	ld		a,16+128
	out		($99),a
  
  ld    b,16                      ;16 colors
  ld    a,(LogoAnimationVar1)     ;palette step
  ld    c,a
  inc   a
  ld    (LogoAnimationVar1),a     ;palette step  
  cp    9
  jr    nz,.NotEnd
  ld    a,4
  ld    (LogoAnimationPhase),a  
  ret

  .NotEnd:
  ld    a,(LogoAnimationVar2)     ;palette step  
  ld    d,a
  add   %0001 0001
  ld    (LogoAnimationVar2),a     ;palette step  

  .loop:                          ;in d=palette step, b=amount of colors
  ld    a,(hl)                    ;0 R2 R1 R0 0 B2 B1 B0
  add   a,d
  bit   3,a
  jr    z,.EndCheckOverflowB
  or    %0000 0111
  .EndCheckOverflowB:
  bit   7,a
  jr    z,.EndCheckOverflowR
  or    %0111 0000
  .EndCheckOverflowR:
  out   ($9a),a                   ;red + blue
  inc   hl
  ld    a,(hl)                    ;0 0 0 0 0 G2 G1 G0
  add   a,c
  bit   3,a
  jr    z,.EndCheckOverflowG
  ld    a,%0000 0111
  .EndCheckOverflowG:
  out   ($9a),a                   ;green
  inc   hl  
  djnz  .loop
  ret
  
LogoAnimationPhase2:              ;non transparant blocks only for a while
  ld    a,1                       ;sPage
  ld    (CopyLogoPart+sPage),a    ;0=transparant blocks, 1=non transparant blocks, 2=black blocks
  
  call  SmallBlock
  call  BigBlock
  ld    a,(LogoAnimationVar1)
  inc   a
  ld    (LogoAnimationVar1),a
  cp    20
  ret   nz
  ld    a,3
  ld    (LogoAnimationPhase),a
  xor   a
  ld    (LogoAnimationVar1),a     ;palette step
  ld    (LogoAnimationVar2),a     ;palette step
  ret

LogoAnimationPhase1:              ;transition from transparant block to non transparant blocks
  ld    a,(LogoAnimationVar2)
  inc   a
  ld    (LogoAnimationVar2),a
  cp    39
  jr    nz,.EndTableCheck
  ld    a,2
  ld    (LogoAnimationPhase),a
  ret
  .EndTableCheck:

  ld    hl,WhichBlocksTable1-1
  ld    d,0
  ld    e,a
  add   hl,de
  ld    a,(hl)
  ld    (CopyLogoPart+sPage),a    ;0=transparant blocks, 1=non transparant blocks, 2=black blocks

  call  SmallBlock
  jp    BigBlock
  
LogoAnimationPhase0:              ;transparant block rotating
  call  screenon
  
  xor   a                         ;sPage
  ld    (CopyLogoPart+sPage),a    ;0=transparant blocks, 1=non transparant blocks, 2=black blocks
  
  call  SmallBlock
  call  BigBlock
  ld    a,(LogoAnimationVar1)
  inc   a
  ld    (LogoAnimationVar1),a
  cp    180
  jr    z,.end
  
  cp    01
  ld    hl,fadeinLogoStep0
  jp    z,setpalette
  cp    04
  ld    hl,fadeinLogoStep1
  jp    z,setpalette
  cp    07
  ld    hl,fadeinLogoStep2
  jp    z,setpalette
  cp    10
  ld    hl,fadeinLogoStep3
  jp    z,setpalette
  cp    13
  ld    hl,fadeinLogoStep4
  jp    z,setpalette
  cp    16
  ld    hl,fadeinLogoStep5
  jp    z,setpalette
  cp    19
  ld    hl,TeamNXTLogoBlackFramePalette
  jp    z,setpalette

  cp    08 + 80
  ld    hl,framestepdarker4Palette
  jr    z,.GoSet4PaletteColors
  cp    15 + 80
  ld    hl,framestepdarker3Palette
  jr    z,.GoSet4PaletteColors
  cp    22 + 80
  ld    hl,framestepdarker2Palette
  jr    z,.GoSet4PaletteColors
  cp    29 + 80
  ld    hl,framestepdarker1Palette
  jr    z,.GoSet4PaletteColors
  cp    36 + 80
  ld    hl,TeamNXTLogoPalette
  jr    z,.GoSet4PaletteColors
  ret
  
  .GoSet4PaletteColors:
  xor   a                         ;start write to this palette color (0)
	out		($99),a
	ld		a,16+128
	out		($99),a  
  call  Set4PaletteColors
  ret
  
  .end:
  ld    a,1
  ld    (LogoAnimationPhase),a
  ret

BigBlock:                         ;rotate big block
  ld    a,(LogoAnimationTimer3)
  inc   a
  cp    6
  jr    nz,.EndCheckLastStepTimer3
  xor   a
  .EndCheckLastStepTimer3:
  ld    (LogoAnimationTimer3),a

  jr    z,.go
  ld    a,(LogoAnimationTimer2)
  dec   a
  ld    (LogoAnimationTimer2),a
  .go:
  
  ld    a,(LogoAnimationTimer2)
  inc   a
  cp    10
  jr    nz,.EndCheckLastStep
  xor   a
  .EndCheckLastStep:
  ld    (LogoAnimationTimer2),a
  add   a,2 + 10
  jp    CopyLogoPartRoutine
  
SmallBlock:                       ;rotate small block
  ld    a,(framecounter)
  and   3
  jr    z,.go
  ld    a,(LogoAnimationTimer1)
  dec   a
  ld    (LogoAnimationTimer1),a
  .go:
  
  ld    a,(LogoAnimationTimer1)
  inc   a
  cp    10
  jr    nz,.EndCheckLastStep
  xor   a
  .EndCheckLastStep:
  ld    (LogoAnimationTimer1),a
  add   a,2
  jp    CopyLogoPartRoutine

SetTransparantOrNonTransparant:  
  ld    a,(Framecounter)
  or    a
  ld    a,1                       ;sPage
  jp    p,.SetsPage
  xor   a  
  .SetsPage:
  ld    (CopyLogoPart+sPage),a
  ret

CopyLogoPartRoutine:
  add   a,a                       ;*2
  ld    b,a
  add   a,a                       ;*4
  add   a,b                       ;*6  
  
  ld    d,0
  ld    e,a
  ld    ix,CopyInstructionsLogo
  add   ix,de

  ld    a,2
  ld    (CopyLogoPart+dPage),a

  ld    a,(ix+0)                  ;sx
  ld    (CopyLogoPart+sx),a
  ld    a,(ix+1)                  ;sy
  ld    (CopyLogoPart+sy),a
  ld    a,(ix+2)                  ;nx
  ld    (CopyLogoPart+nx),a
  ld    a,(ix+3)                  ;ny
  ld    (CopyLogoPart+ny),a
  ld    a,(ix+4)                  ;dx
  ld    (CopyLogoPart+dx),a
  ld    a,(ix+5)                  ;dy
  ld    (CopyLogoPart+dy),a

  ld    hl,CopyLogoPart

  ld    a,(CopyLogoPart+sPage)
  cp    2
  jp    nz,DoCopy

  xor   a
  ld    (CopyLogoPart+sx),a
  ld    (CopyLogoPart+sy),a
  jp    DoCopy
  
TeamNXTLogoPalette:
  incbin "..\grapx\TeamNXTLogo\TeamNXTLogoPalette.PL" ;file palette 
TeamNXTLogoTunnelBlackPalette:
  incbin "..\grapx\TeamNXTLogo\TeamNXTLogoTunnelBlackPalette.PL" ;file palette 

CleanPage2:
  db    000,000,000,000   ;sx,--,sy,spage
  db    000,000,000,002   ;dx,--,dy,dpage
  db    000,001,000,001   ;nx,--,ny,--
  db    %1111 1111,000,$C0       ;fill   
  
CopyInstructionsLogo: ;sx, sy, nx, ny, dx, dy
;0: total logo transparant
  db    092,144,108,050,050,080
  db    200,135,056,065,158,071
;2: small transparant block
  db    000,118,032,026,052,101
  db    032,118,032,026,052,101
  db    064,118,032,026,052,101
  db    096,118,032,026,052,101
  db    128,118,032,026,052,101
  db    160,118,032,026,052,101
  db    000,144,032,026,052,101
  db    032,144,032,026,052,101
  db    000,170,032,026,052,101
  db    032,170,032,026,052,101
;12: big transparant block
  db    000,000,046,059,158,074
  db    046,000,046,059,158,074
  db    092,000,046,059,158,074
  db    138,000,046,059,158,074
  db    184,000,046,059,158,074
  db    000,059,046,059,158,074
  db    046,059,046,059,158,074
  db    092,059,046,059,158,074
  db    138,059,046,059,158,074
  db    184,059,046,059,158,074 