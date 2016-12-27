
namespace Tokamak
    module Core =

        type DataType
        type Block
        type Statement
        type Expression

        ///////////////////////////////////////////////////////////////////
        ///                                                             ///
        ///                     -- Reactor Core --                      /// 
        ///                                                             ///
        //////////////////////////////////////////////////////////////////////////////////////
                           ///                                                             ///
                           ///            Contains AST of parsed script                    /// 
                           ///                                                             ///
                           ///////////////////////////////////////////////////////////////////
        type ReactorCore =
            new : Block -> ReactorCore
            member block : Block

            /////////////////////////////////////////////////////////////////////////////////////////
            /// 1. Incident report.                                                               ///
            ///    After confinement failure at site XB302, new procedures have been implemented  ///
        /////////////////////////////////////////////////////////////////// secure the outer      ///
        ///                                                             /////////////////////////////////
        ///            -- Plasma Confinement Unity --                   /// field is bound by steel   ///
        ///                                                             /// ruptured container. The   ///
        ///////////////////////////////////////////////////////////////////ore temp. will reach       ///
            ///    procudres ///     several million degrees centigrade.                              ///
            ///    if the con////////////////////////////////////////////////////////////////////////////
            ///    casualty rate should be decreased.                                             ///
            /////////////////////////////////////////////////////////////////////////////////////////
        type PlasmaConfinementUnit<'T> =
            abstract member Extract: unit -> 'T
        

        type IntegerConfinementUnit =
            new : Expression -> IntegerConfinementUnit
            member R : int64
            interface PlasmaConfinementUnit<int64>

        type FloatConfinementUnit =
            new : Expression -> FloatConfinementUnit
            member R : double
            interface PlasmaConfinementUnit<double>

        type LiteralConfinementUnit =
            new : Expression -> LiteralConfinementUnit
            member R : string
            interface PlasmaConfinementUnit<string>

        type BoolConfinementUnit =
            new : Expression -> BoolConfinementUnit
            member R : bool
            interface PlasmaConfinementUnit<bool>

        type ArrayConfinementUnit =
            new : Expression -> ArrayConfinementUnit
            member R : Expression list
            interface PlasmaConfinementUnit<Expression list>

        type ExternalVariable =
            new : string * DataType -> ExternalVariable
            member name :  string
            member value : Expression


                                        //////////////////////////////////////////////////////////////////////
                                        ///                          --Status Report--
                                        /// Status: Shutdown
                                        /// Cause:  Core breach cascade reaction.
        ///////////////////////////////////////////////////////////////////
        ///                                                             ///
        ///               -- Compiler. Fusion Reactor --                /// after several minutes. Previously 
        ///                                                             /// microfissures were found by the
        /////////////////////////////////////////////////////////////////// leaking plasma into the outer
        type Compiler =                 /// chamber. On realization, site XB302 was evacuated, and emergency
            new : unit -> Compiler      /// protocol E0302 was initiated. The uncontained reaction shot 
                                        /// super-heated plasma into the site containment facility, causing
            member compile : string -> Expression
            member compileWithArgs : string  * ExternalVariable array -> Expression
            member EjectCore : string -> ReactorCore
            member IgniteCore : ReactorCore -> Expression
            member EvaluateExpression : Expression -> Expression
            member AddExternalCall : string * System.Action -> unit
                                        /// impossible to be recovered from the debris.  
                                        ///
                                        /// Gordan Freeman.
                                        //////////////////////////////////////////////////////////////////////