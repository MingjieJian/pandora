      subroutine YEAST
     $(INDADR,XCBL,KTRN,JNU,XJNU,ADD,XKPC,BBC,SIG)
C
C     Rudolf Loeser, 1982 Jul 19
C---- Retrieves and sets up line background continuum data:
C     JNU = true: - Jnu
C     ADD = true: - KPC,BBC,SIG.
C
C---- KTRN is "K for this transition", as kept in the corresponding
C     Line Intensity Data Block; it is the number of points in the
C     table of frequency values used to compute profile integrals.
C     Thus it also is the number of Background Continuum Data Blocks
C     for this transition.
C
C     (This is version 3 of YEAST.)
C     !DASH
      save
C     !DASH
      real*8 BBC, SIG, XCBL, XJNU, XKPC
      integer INDADR, J, KKBNMS, KKCAPP, KKJNU, KKMULT, KKSIGS, KTRN, N
      logical ADD, JNU
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(13),KKJNU )
      equivalence (KKK( 2),KKMULT)
      equivalence (KKK(29),KKCAPP)
      equivalence (KKK(16),KKBNMS)
      equivalence (KKK(45),KKSIGS)
C     !DASH
C     !EJECT
      external LEYTE, MOVE1, ERSILIA, HI, BYE
C
C               XCBL(Miklen), INDADR(KTRN), XJNU(N,KTRN), XKPC(N,KTRN),
      dimension XCBL(*),      INDADR(*),    XJNU(N,*),    XKPC(N,*),
C
C               BBC(N,KTRN), SIG(N,KTRN)
     $          BBC(N,*),    SIG(N,*)
C
      call HI ('YEAST')
C     !BEG
      do 100 J = 1,KTRN
C----   Read data block
        call LEYTE     (XCBL, MIKLEN, INDADR(J))
C
        if(JNU) then
C----     Get Jnu
          call MOVE1   (XCBL(KKJNU), N, XJNU(1,J))
        end if
C
        if(ADD) then
C----     Optional other stuff
          call ERSILIA (N, XKPC(1,J), BBC(1,J), SIG(1,J), XCBL(KKMULT),
     $                  XCBL(KKCAPP), XCBL(KKBNMS), XCBL(KKSIGS))
        end if
C
  100 continue
C     !END
      call BYE ('YEAST')
C
      return
      end
