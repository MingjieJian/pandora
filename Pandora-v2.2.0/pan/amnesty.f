      subroutine AMNESTY
     $(LU,N,NL,Z,GVL,GVO)
C
C     Rudolf Loeser, 2003 Oct 28
C---- Prints details of smoothing of GNVL.
C     !DASH
      save
C     !DASH
      real*8 GVL, GVO, Z
      integer I, INDX, J, KODE, LU, N, NL
      character RAT*16
C     !DASH
      external YANEST, ABJECT, LINER, HI, BYE
C
C               Z(N), GVL(N,NL), GVO(N,NL)
      dimension Z(*), GVL(N,*),  GVO(N,*)
C
      call HI ('AMNESTY')
C     !BEG
      if(LU.gt.0) then
        KODE = 0
        do 104 J = 1,NL
          do 103 I = 1,N
            if(GVL(I,J).ne.GVO(I,J)) then
              if(KODE.eq.0) then
                call ABJECT (LU)
                KODE = KODE+1
                write (LU,100)
  100           format(' ','Details of smoothing of GNVL.')
              end if
              if(KODE.eq.1) then
                call LINER  (2, LU)
                KODE = KODE+1
                write(LU,101) J
  101           format(' ','For level',I4//
     $                 ' ',4X,'i',15X,'Z',17X,'old',9X,'new/old',
     $                     17X,'new')
                INDX = -1
              end if
              if(I.ne.(INDX+1)) then
                call LINER  (1, LU)
              end if
              INDX = I
              call YANEST   (GVL(INDX,J), GVO(INDX,J), RAT)
              write (LU,102) INDX,Z(INDX),GVO(INDX,J),RAT,GVL(INDX,J)
  102         format(' ',I5,1PE16.8,E20.8,A16,E20.8)
            end if
  103     continue
          KODE = 1
  104   continue
      end if
C     !END
      call BYE ('AMNESTY')
C
      return
      end
