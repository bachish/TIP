package tip.analysis

import tip.ast.AstNodeData.DeclarationData
import tip.ast._
import tip.cfg._
import tip.lattices.{MapLattice, PowersetLattice}
import tip.solvers.{SimpleMapLatticeFixpointSolver, SimpleWorklistFixpointSolver}


/**
 * Base class for reaching definitions analysis.
 */
abstract class ReachingDefAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(true) {

  //only assignments are stored
  val lattice: MapLattice[CfgNode, PowersetLattice[AAssignStmt]] = new MapLattice(new PowersetLattice())
  val domain: Set[CfgNode] = cfg.nodes

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case _: CfgFunEntryNode => Set()
      case r: CfgStmtNode =>
        r.data match {
          case as: AAssignStmt =>
            as.left match {
              // [|x = E;|] = removedefs(JOIN(n), x ) ∪ {x = E }
              case id: AIdentifier =>
                s.filter(decl => decl.left.asInstanceOf[AIdentifier].name != id.name) + as
              case _ => ???
            }
          //[|n|] = JOIN(n)
          case _ => s
        }
      case _ => s
    }
}

/**
 * Available expressions analysis that uses the simple fipoint solver.
 */
class ReachingDefAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
  extends ReachingDefAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with ForwardDependencies

/**
 * Available expressions analysis that uses the worklist solver.
 */
class ReachingDefAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
  extends ReachingDefAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies
