      subroutine HYDRU
     $(N,Z,KZXST,TE,TES,TER,IQUET,KTECH,JSSV,SCTA,SCTS,W,IW,NO)
C
C     Rudolf Loeser, 2001 Nov 29
C---- Checks  and edits the TE table.
C     (This is version 3 of HYDRU.)
C     !DASH
      save
C     !DASH
      real*8 ONE, SCTA, SCTS, TE, TER, TES, W, Z
      integer INDX, IQUET, IW, JSSV, KRD, KSM, KTECH, KZXST, LGT, LUEO,
     $        N, NO
      logical ISO, NEW, OK
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external KONSTD, MESHED, VECOUT, PITTED, SMOOTH, ROUND, MASHED,
     $         PETIT, ABORT, PLUSD, MOVE1, HI, BYE
C
      dimension W(*), IW(*)
C
C               TE(N), TES(N), TER(N), Z(N)
      dimension TE(*), TES(*), TER(*), Z(*)
C
      data INDX /0/
C
      call HI ('HYDRU')
C     !BEG
C---- Check the TE values
      call PLUSD      (TE, 1, N, LGT)
      if(LGT.lt.N) then
        call MESHED   ('HYDRU', 1)
        write (LUEO,100)
  100   format(' ','All input values of TE must be greater than zero.')
        call VECOUT   (LUEO, TE, N, 'TE')
        call ABORT
      end if
C
      call KONSTD     (TE, 1, N, TE(1), ISO)
      if(ISO.or.(KZXST.eq.0)) then
C----   Skip smoothing in isothermal case, or no input Z
        goto 103
      end if
C     !EJECT
C---- Smooth the TE values
      call MOVE1      (TE, N, TES)
      call SMOOTH     (Z, TES, N, 'lin', 'Input TE(Z)', INDX, W, IW,
     $                 KSM, OK)
      if(OK) then
C
C----   Round the TE values
        call MOVE1    (TES, N, TER)
        call ROUND    (TER, N, ONE, KRD)
C
        NEW = (KSM.gt.0).or.(KRD.gt.0)
        if(NEW) then
C----     Print details of editing
          call PETIT  (N, Z, TE, TES, TER, IQUET, NO)
        else
          call MESHED ('HYDRU', 3)
          write (NO,101)
  101     format(' ','No input-TE values required editing.')
          call MASHED ('HYDRU')
        end if
C
        if(NEW.and.(IQUET.gt.0)) then
C----     Adopt the edited TE values
          call MOVE1  (TER, N, TE)
          KTECH = 1
        end if
C
      else
        call MESHED   ('HYDRU', 3)
        write (LUEO,102)
  102   format(' ','Smoothing of TE values failed: editing skipped.')
        call MASHED   ('HYDRU')
      end if
C
  103 continue
C
      if(JSSV) then
        if(KZXST.gt.0) then
C----     Add variation due to shock
          call PITTED (N, Z, TE, JSSV, SCTA, SCTS, NO)
        else
          call MESHED ('HYDRU', 3)
          write (LUEO,104)
  104     format(' ','Shock component of TE could not be computed ',
     $               'because no Z-table is given.')
          call MASHED ('HYDRU')
        end if
      end if
C     !END
      call BYE ('HYDRU')
C
      return
      end
