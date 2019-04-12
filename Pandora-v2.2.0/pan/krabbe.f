      subroutine KRABBE
     $(DUMP,K,A,MORD,JP,VP,V,GII,IGII)
C
C     Rudolf Loeser, 2004 Nov 22
C---- Debug output for GITTAR.
C     !DASH
      save
C     !DASH
      real*8 A, GII, V, VP
      integer IGII, JP, K, LUEO, MORD
      logical DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, VECOUT, HI, BYE
C
C               V(K), GII(K)
      dimension V(*), GII(*)
C
      call HI ('KRABBE')
C     !BEG
      if(DUMP) then
        call LINER  (1, LUEO)
C
        if(IGII.eq.1) then
          write (LUEO,100) 'Approximate',A,VP,JP,MORD
        else
          write (LUEO,100) 'Accurate',A,VP,JP
        end if
  100   format(' ',A,' GII calculation for A =',1PE14.6,', v'' =',
     $             E14.6,5X,'(k'' =',I6,:,', order =',I1)
C
        call VECOUT (LUEO, V, K, 'v')
        call VECOUT (LUEO, GII, K, 'GII')
      end if
C     !END
      call BYE ('KRABBE')
C
      return
      end
