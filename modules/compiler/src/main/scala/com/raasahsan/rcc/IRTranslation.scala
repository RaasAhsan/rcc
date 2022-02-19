package com.raasahsan.rcc

// Performs a translation between the C syntax tree and the
// RCC internal representation.
// This translation performs some significant operations in order to simplify
// the downstream pipeline:
// 1. More precise declarator information (e.g. function definitions)
// 2. Variable declaration expansion
// 3. Specifiers to type mapping
object IRTranslation {

  def translateTranslationUnit(unit: AST.TranslationUnit): IR.Module =
    IR.Module(unit.externalDeclarations.toList.map(translateExternalDeclaration))

  def translateExternalDeclaration(decl: AST.ExternalDeclaration): IR.ModuleDeclaration =
    decl match {
      case AST.ExternalDeclaration.Declaration(decl) =>
        ???
      case AST.ExternalDeclaration.FunctionDefinition(fd) =>
        IR.ModuleDeclaration.FunctionDefinition(translateFunctionDefinition(fd))
    }

  def translateFunctionDefinition(fd: AST.FunctionDefinition): IR.FunctionDefinition = {
    // TODO: assert at most one specifier?
    val storageClass = fd.specifiers.flatMap(extractStorageClass).map(translateStorageClass)

    val (name, functionParams) = fd.declarator.directDeclarator match {
      case AST.DirectDeclarator.FunctionDeclarator(dd, params) =>
        val identifier = extractIdentifierFromDirectDeclarator(dd)

        val functionParams = params.parameterList.parameters.toList.map {
          case AST.ParameterDeclaration.Declarator(specifiers, declOpt) =>
            val decl =
              declOpt.get // TODO: an identifier is expected here, except if the specifier is void?
            val paramTpe = extractTypeFromDeclaration(specifiers, decl).get
            val paramName = extractIdentifierFromDirectDeclarator(decl.directDeclarator)
            IR.FunctionParameter(paramTpe, paramName)
        }

        identifier -> functionParams
      case AST.DirectDeclarator.Identifiers(dd, None) =>
        val identifier = extractIdentifierFromDirectDeclarator(dd)
        identifier -> Nil
      case x => throw new IllegalStateException(s"function declarator expected, got $x")
    }

    val returnTpe = fd.specifiers.flatMap(extractTypeFromSpecifiers).get

    val block = IR.Block(
      fd.statements.declarationList
        .map(_.declarations.toList)
        .getOrElse(Nil)
        .flatMap(translateDeclaration),
      fd.statements.statementList.map(_.statements.toList).getOrElse(Nil).map(translateStatement)
    )

    IR.FunctionDefinition(storageClass, name, returnTpe, Some(functionParams), block)
  }

  def translateDeclaration(decl: AST.Declaration): List[IR.Declaration] =
    decl.initDeclaratorList.map(_.declarators.toList).getOrElse(Nil).map { initDecl =>
      val identifier = extractIdentifierFromDirectDeclarator(initDecl.declarator.directDeclarator)
      val storageClass = extractStorageClass(decl.specifiers).map(translateStorageClass)
      val tpe = extractTypeFromDeclaration(decl.specifiers, initDecl.declarator)
        .getOrElse(throw new RuntimeException("no type found"))
      IR.Declaration(
        storageClass,
        Nil,
        identifier,
        tpe,
        initDecl.initializer.map(translateInitializer)
      )
    }

  def translateInitializer(init: AST.Initializer): IR.Initializer =
    init match {
      case AST.Initializer.Expression(expr) => IR.Initializer.Expression(translateExpression(expr))
      case _                                => ???
    }

  def translateStatement(stmt: AST.Statement): IR.Statement =
    stmt match {
      case AST.Statement.Compound(compound) =>
        IR.Statement.Compound(
          IR.Block(
            compound.declarationList
              .map(_.declarations.toList)
              .getOrElse(Nil)
              .flatMap(translateDeclaration),
            compound.statementList.map(_.statements.toList).getOrElse(Nil).map(translateStatement)
          )
        )
      case AST.Statement.Expression(expr) =>
        IR.Statement.Expression(
          IR.ExpressionStatement(
            expr.expr.map(translateExpression)
          )
        )
      case AST.Statement.Labeled(labeled) =>
        IR.Statement.Labeled(IR.LabeledStatement())
      case AST.Statement.Iteration() => IR.Statement.Iteration()
      case AST.Statement.Selection(select) =>
        IR.Statement.Selection(
          select match {
            case AST.SelectionStatement.If(condition, consequent, alternative) =>
              IR.SelectionStatement.If(
                translateExpression(condition),
                translateStatement(consequent),
                alternative.map(translateStatement)
              )
          }
        )
      case AST.Statement.Jump(jump) =>
        IR.Statement.Jump(
          jump match {
            case AST.JumpStatement.Break    => IR.JumpStatement.Break
            case AST.JumpStatement.Continue => IR.JumpStatement.Continue
            case AST.JumpStatement.Return(expr) =>
              IR.JumpStatement.Return(expr.map(translateExpression))
            case AST.JumpStatement.Goto(label) => IR.JumpStatement.Goto(translateIdentifier(label))
          }
        )
    }

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

  def translateExpression(expr: AST.Expression): IR.Expression =
    expr match {
      case AST.Expression.Constant(const)    => IR.Expression.Constant(translateConstant(const))
      case AST.Expression.Identifier(ident)  => IR.Expression.Identifier(translateIdentifier(ident))
      case AST.Expression.StringLiteral(str) => IR.Expression.StringLiteral(str.value)
      case AST.Expression.Assignment(lhs, rhs) =>
        IR.Expression.Assignment(translateExpression(lhs), translateExpression(rhs))
      case AST.Expression.Plus(lhs, rhs) =>
        IR.Expression.Plus(translateExpression(lhs), translateExpression(rhs))
      case AST.Expression.Minus(lhs, rhs) =>
        IR.Expression.Minus(translateExpression(lhs), translateExpression(rhs))
      case AST.Expression.Times(lhs, rhs) =>
        IR.Expression.Times(translateExpression(lhs), translateExpression(rhs))
      case AST.Expression.Divide(lhs, rhs) =>
        IR.Expression.Divide(translateExpression(lhs), translateExpression(rhs))
      case AST.Expression.Modulo(lhs, rhs) =>
        IR.Expression.Modulo(translateExpression(lhs), translateExpression(rhs))
      case AST.Expression.FunctionCall(lhs, args) =>
        IR.Expression.FunctionCall(
          translateExpression(lhs),
          args.map(_.args.toList).getOrElse(Nil).map(translateExpression)
        )
      case AST.Expression.ArrayGet(lhs, index) =>
        IR.Expression.ArrayGet(translateExpression(lhs), translateExpression(index))
      case AST.Expression.Reference(expr)   => IR.Expression.Reference(translateExpression(expr))
      case AST.Expression.Dereference(expr) => IR.Expression.Dereference(translateExpression(expr))
      case AST.Expression.Cast(typeName, expr) =>
        IR.Expression.Cast(translateTypeName(typeName), translateExpression(expr))
    }

  def translateConstant(const: AST.Constant): IR.Constant =
    const match {
      case AST.Constant.IntegerConstant(i) => IR.Constant.IntegerConstant(i)
    }

  def translateTypeName(typeName: AST.TypeName): IR.TypeName =
    IR.TypeName(
      typeName.specifierQualifiers.map(translateTypeSpecifierOrQualifier),
      typeName.abstractDeclarator.map(translateAbstractDeclarator)
    )

  def translateTypeSpecifierOrQualifier(
      in: AST.TypeSpecifierOrQualifier
  ): IR.TypeSpecifierOrQualifier =
    in match {
      case AST.TypeSpecifierOrQualifier.Qualifier(q) =>
        IR.TypeSpecifierOrQualifier.Qualifier(translateTypeQualifier(q))
      case AST.TypeSpecifierOrQualifier.Specifier(s) =>
        IR.TypeSpecifierOrQualifier.Specifier(translateTypeSpecifier(s))
    }

  def translateAbstractDeclarator(decl: AST.AbstractDeclarator): IR.AbstractDeclarator =
    IR.AbstractDeclarator(translatePointer(decl.pointer))

  def translatePointer(ptr: AST.Pointer): IR.Pointer =
    IR.Pointer(
      ptr.typeQualifiers.map(_.qualifiers.toList).getOrElse(Nil).map(translateTypeQualifier),
      ptr.pointer.map(translatePointer)
    )

  def translateTypeQualifier(q: AST.TypeQualifier): IR.TypeQualifier =
    q match {
      case AST.TypeQualifier.Const    => IR.TypeQualifier.Const
      case AST.TypeQualifier.Volatile => IR.TypeQualifier.Volatile
    }

  def translateTypeSpecifier(s: AST.TypeSpecifier): IR.TypeSpecifier =
    s match {
      case AST.TypeSpecifier.Char     => IR.TypeSpecifier.Char
      case AST.TypeSpecifier.Double   => IR.TypeSpecifier.Double
      case AST.TypeSpecifier.Float    => IR.TypeSpecifier.Float
      case AST.TypeSpecifier.Int      => IR.TypeSpecifier.Int
      case AST.TypeSpecifier.Long     => IR.TypeSpecifier.Long
      case AST.TypeSpecifier.Short    => IR.TypeSpecifier.Short
      case AST.TypeSpecifier.Signed   => IR.TypeSpecifier.Signed
      case AST.TypeSpecifier.Unsigned => IR.TypeSpecifier.Unsigned
      case AST.TypeSpecifier.Void     => IR.TypeSpecifier.Void
      case _                          => ???
    }

  private def extractIdentifierFromDirectDeclarator(dd: AST.DirectDeclarator): IR.Identifier =
    dd match {
      case AST.DirectDeclarator.Identifier(i) => translateIdentifier(i)
      case _ => throw new IllegalStateException("identifier expected for parameter")
    }

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

  private val specifierMapping: Map[Set[AST.TypeSpecifier], Type] = Map(
    Set(AST.TypeSpecifier.Int) -> Type.Int,
    Set(AST.TypeSpecifier.Char) -> Type.Char,
    Set(AST.TypeSpecifier.Unsigned, AST.TypeSpecifier.Int) -> Type.UnsignedInt
  )

  // TODO: qualified types?
  private def extractTypeFromSpecifiers(specifiers: AST.DeclarationSpecifiers): Option[Type] = {
    val typeSpecifiers = specifiers.specifiers.toList.collect {
      case AST.DeclarationSpecifier.TypeSpecifier(ts) => ts
    }.toSet
    specifierMapping.get(typeSpecifiers)
  }

}