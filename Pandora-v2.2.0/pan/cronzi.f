      subroutine CRONZI
     $(NO,N,NL,NT,YBAR,RHO,QHI,AW,FCE,KIJ)
C
C     Rudolf Loeser, 2003 Jun 17
C---- Prints arrays of basic depth-dependent transition input data.
C     (This is version 2 of CRONZI.)
C     !DASH
      save
C     !DASH
      real*8 AW, FCE, ONE, QHI, RHO, YBAR
      integer KIJ, N, NL, NNT, NO, NT
      logical ISONE, ISZER
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external LINER, SCRIBE, NAUGHTD, KONSTD, FENSTER, HI, BYE
C
C               YBAR(N,NT), RHO(N,NT), QHI(N,NT), AW(N,NT), KIJ(NL,NL),
      dimension YBAR(*),    RHO(*),    QHI(*),    AW(*),    KIJ(*),
C
C               FCE(N,NT)
     $          FCE(*)
C
      call HI ('CRONZI')
C     !BEG
      if(NO.gt.0) then
        NNT = N*NT
C
        call LINER    (3, NO)
        write (NO,100)
  100   format(' ','********** JBAR -- Mean Intensity')
        call NAUGHTD  (YBAR, 1, NNT, ISZER)
        if(ISZER) then
          call LINER  (1, NO)
          write (NO,101)
  101     format(' ','All zero.')
        else
          call SCRIBE (YBAR, 'NT', KIJ, 1, N, N, NL, NO, 1)
        end if
C     !EJECT
        call LINER    (3, NO)
        write (NO,102)
  102   format(' ','********** RHO -- Net Radiative Bracket')
        call NAUGHTD  (RHO, 1, NNT, ISZER)
        call KONSTD   (RHO, 1, NNT, ONE, ISONE)
        if(ISZER) then
          call LINER  (1, NO)
          write (NO,101)
        else if(ISONE) then
          call LINER  (1, NO)
          write (NO,103)
  103     format(' ','All one.')
        else
          call SCRIBE (RHO , 'NT', KIJ, 1, N, N, NL, NO, 1)
        end if
C
        call LINER    (3, NO)
        write (NO,104)
  104   format(' ','********** CHI')
        call NAUGHTD  (QHI, 1, NNT, ISZER)
        if(ISZER) then
          call LINER  (1, NO)
          write (NO,101)
        else
          call SCRIBE (QHI , 'NT', KIJ, 1, N, N, NL, NO, 1)
        end if
C
        call LINER    (3, NO)
        write (NO,105)
  105   format(' ','********** AW -- Diagonal of WN Weight Matrix')
        call NAUGHTD  (AW, 1, NNT, ISZER)
        if(ISZER) then
          call LINER  (1, NO)
          write (NO,101)
        else
          call SCRIBE (AW  , 'NT', KIJ, 1, N, N, NL, NO, 1)
        end if
C
        call FENSTER  (N, NL, KIJ, FCE, NO)
      end if
C     !END
      call BYE ('CRONZI')
C
      return
      end
