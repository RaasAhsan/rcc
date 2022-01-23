package com.raasahsan.rcc

// Performs a translation between the C syntax tree and the
// RCC internal representation.
// This translation performs some significant operations in order to simplify
// the downstream pipeline:
// 1. More precise declarator information (e.g. function definitions)
// 2. Variable declaration expansion
// 3. Specifiers to type mapping
object ASTToIR {

  def translateTranslationUnit(unit: AST.TranslationUnit): IR.Module =
    IR.Module(unit.externalDeclarations.toList.map(translateExternalDeclaration))

  def translateExternalDeclaration(decl: AST.ExternalDeclaration): IR.ModuleDeclaration = ???

  def translateFunctionDefinition(fd: AST.FunctionDefinition): IR.FunctionDefinition = {
    // TODO: assert at most one specifier?
    val storageClass = fd.specifiers.flatMap(extractStorageClass).map(translateStorageClass)

    val (name, functionParams) = fd.declarator.directDeclarator match {
      case AST.DirectDeclarator.FunctionDeclarator(dd, params) =>
        val identifier = dd match {
          case AST.DirectDeclarator.Identifier(i) => translateIdentifier(i)
          case _ => throw new IllegalStateException("identifier expected")
        }

        val functionParams = params.parameterList.parameters.toList.map {
          case AST.ParameterDeclaration.Declarator(specifiers, decl) =>
            val paramTpe = extractTypeFromDeclaration(specifiers, decl).get
            val paramName = decl.directDeclarator match {
              case AST.DirectDeclarator.Identifier(i) => translateIdentifier(i)
              case _ => throw new IllegalStateException("identifier expected for parameter")
            }
            IR.FunctionParameter(paramTpe, paramName)
        }

        identifier -> functionParams
      case _ => throw new IllegalStateException("function declarator expected")
    }

    val returnTpe = fd.specifiers.flatMap(extractTypeFromSpecifiers).get

    val block = IR.Block(
      fd.statements.declarationList
        .map(_.declarations.toList)
        .getOrElse(Nil)
        .map(translateDeclaration),
      fd.statements.statementList.map(_.statements.toList).getOrElse(Nil).map(translateStatement)
    )

    IR.FunctionDefinition(storageClass, name, returnTpe, Some(functionParams), block)
  }

  def translateDeclaration(decl: AST.Declaration): IR.Declaration = ???

  def translateStatement(stmt: AST.Statement): IR.Statement = ???

  def translateStorageClass(sc: AST.StorageClassSpecifier): IR.StorageClassSpecifier =
    sc match {
      case AST.StorageClassSpecifier.Auto     => IR.StorageClassSpecifier.Auto
      case AST.StorageClassSpecifier.Extern   => IR.StorageClassSpecifier.Extern
      case AST.StorageClassSpecifier.Register => IR.StorageClassSpecifier.Register
      case AST.StorageClassSpecifier.Static   => IR.StorageClassSpecifier.Static
      case AST.StorageClassSpecifier.Typedef  => IR.StorageClassSpecifier.Typedef
    }

  def translateIdentifier(ident: AST.Identifier): IR.Identifier =
    IR.Identifier(ident.value)

  // Not for function definitions?
  // TODO: array types
  private def extractTypeFromDeclaration(
      specifiers: AST.DeclarationSpecifiers,
      declarator: AST.Declarator
  ): Option[Type] = {
    val baseTpe = extractTypeFromSpecifiers(specifiers)
    // TODO: multiple pointer nestings
    baseTpe.map(tpe => declarator.pointer.fold(tpe)(_ => Type.Pointer(tpe)))
  }

  private def extractStorageClass(
      specifiers: AST.DeclarationSpecifiers
  ): Option[AST.StorageClassSpecifier] =
    specifiers.specifiers.toList.collect {
      case AST.DeclarationSpecifier.StorageClassSpecifier(sc) => sc
    }.headOption

  private def extractTypeFromSpecifiers(specifiers: AST.DeclarationSpecifiers): Option[Type] = {

    ???
  }

}
