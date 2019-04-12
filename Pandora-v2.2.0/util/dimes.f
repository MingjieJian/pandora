      program dimes
C
C     Rudolf Loeser, 2002 Apr 10
C
C---- Separates a file of ion abundance ratios produced by census
C     into separate files, one per ion.
C     !BDECL
      integer NI,LU,NOUT,NF,NX
      character LINE*80
      logical KILROY
C     !EDECL
C     !DASH
      parameter (NX=9)
      dimension NOUT(NX)
C
      data NI,NOUT /63,  64, 65, 66, 67, 68, 69, 70, 71, 72/
C     !beg
      NF = 1
      LU = NOUT(NF)
      KILROY = .true.
C
      rewind NI
  100 continue
C
        read (NI,101,end=102) LINE
  101   format(A80)
C
        if(KILROY) then
          rewind LU
          KILROY = .false.
        end if
        write (LU,101) LINE
C
        if(LINE(:4).eq.') > ') then
          NF = NF+1
          if(NF.gt.NX) then
            goto 102
          end if
          LU = NOUT(NF)
          KILROY = .true.
        end if
C
      goto 100
C
  102 continue
      stop 'dimes: done'
C     !end
      end
