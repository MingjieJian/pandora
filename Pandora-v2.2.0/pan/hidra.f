      subroutine HIDRA
     $(Z,KZXST,HJ,N,JH1,JH2,R1N,IQSFS,IQGDS,IQUTR)
C
C     Rudolf Loeser, 1981 Mar 03
C---- Computes multiplier for photoionization rates.
C     (This is version 6 of HIDRA.)
C     !DASH
      save
C     !DASH
      real*8 D, HALF, HJ, ONE, R1N, T, X1, X2, X3, Z, ZERO
      integer I, IQGDS, IQSFS, IQUTR, J1, J2, J3, J4, JH1, JH2, KZXST,
     $        LUEO, N, NF, NOION
      logical DILUTE
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 94),NOION)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external  SET1, MESHED, MASHED, HI, BYE
      intrinsic min, max
C
C               HJ(N), Z(N)
      dimension HJ(*), Z(*)
C
      call HI ('HIDRA')
C     !BEG
      if(NOION.le.0) then
        if(JH2.le.0) then
          call SET1       (HJ, N, ONE)
          if(IQUTR.gt.0) then
            call MESHED   ('HIDRA', 3)
            write (LUEO,100) JH2,R1N
  100       format(' ','Note that "Photoionization Rates Multiplier" ',
     $                 'MULT affects the computed values of RK ',
     $                 'in this run because option USETRIN=ON.'/
     $             ' ','The input values of the depth indices JH1 ',
     $                 'and JH2 control the calculation of MULT ',
     $                 '(which is set = 1 by default);'/
     $             ' ','an explanation appears among the  N O T E S  ',
     $                 'of the ATMOSPHERE printout when JH2 > 0.'/
     $             ' ','In this run, JH2 =',I5,' and R1N =',1PE16.8)
            call MASHED   ('HIDRA')
          end if
        else
C
          if(JH2.eq.1) then
            call SET1     (HJ, N, ONE)
          else
            if(JH1.gt.JH2) then
              call MESHED ('HIDRA', 3)
              write (LUEO,101) JH1,JH2
  101         format(' ','When JH2 > 1, then JH1 must not exceed JH2.'//
     $               ' ','In this run, JH1 =',I5,' and JH2 =',I5)
              call MASHED ('HIDRA')
              call SET1   (HJ, N, ONE)
            else
              J1 = max(1,JH1)
              J2 = J1+1
              J4 = min(JH2,N)
              J3 = J4-1
              call SET1  (HJ(1),J1,HALF)
C
              if(J3.ge.J2) then
                do 102 I = J2,J3
                  X1 = J4-I
                  X2 = I-J1
                  X3 = J4-J1
                  HJ(I) = (HALF*X1+X2)/X3
  102           continue
              end if
C
              NF = N-J4+1
              call SET1  (HJ(J4),NF,ONE)
            end if
          end if
C     !EJECT
          DILUTE = ((IQSFS.le.0).and.(IQGDS.gt.0)).or.(IQSFS.gt.0)
          if(DILUTE.and.(R1N.ne.ZERO)) then
            if(KZXST.gt.0) then
              do 103 I = 1,N
                D = (Z(N)-Z(I))/R1N
                T = sqrt(ONE-((ONE/(ONE+D))**2))
                HJ(I) = HJ(I)*(ONE-T)
  103         continue
C
            else
              call MESHED  ('HIDRA', 3)
              write (LUEO,104)
  104         format(' ','Spherical dilution part of photoionization ',
     $                   'rates multiplier cannot be computed because ',
     $                   'no input Z-table is given.')
              call MASHED  ('HIDRA')
            end if
          end if
        end if
      end if
C     !END
      call BYE ('HIDRA')
C
      return
      end
