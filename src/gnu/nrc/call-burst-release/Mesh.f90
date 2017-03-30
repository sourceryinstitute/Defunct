MODULE Mesh
    USE Kinds
    USE Conversions
    USE FGR_Mesh, ONLY : FGR_Node, ANS54_Node, FRAPFGR_Node, Massih_Node, IFBA_Coating
    USE Variables, ONLY : nmesh
    USE CoolantVars, ONLY : Coolant_Channel
    USE Grid_Index
!   USE MatLib
    IMPLICIT NONE
    !>@brief
    !> Module Mesh contains the subroutines that define the mesh used in FAST for the conduction and FGR solutions
    !> This module also contains the master list of node-dependent derived types, such as Thermal_Mesh, FGR_Elements, ZrB2
    !> and FuelRod
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 11/15/2016
    
    PRIVATE
    PUBLIC :: FGR_Elements, ZrB2

    CLASS (FGR_Node), DIMENSION(:,:), ALLOCATABLE, SAVE :: FGR_Elements
    CLASS (IFBA_Coating), DIMENSION(:), ALLOCATABLE, SAVE :: ZrB2
    
END MODULE Mesh
