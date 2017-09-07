/*the below are the scala functions to generate the required json for the dtree visualisation*/

    def splitToString(split: org.apache.spark.ml.tree.Split, left: Boolean): String = {
      val featureStr = s"feature ${split.featureIndex}"
      split match {
        case contSplit: org.apache.spark.ml.tree.ContinuousSplit =>
          if (left) {
            s"$featureStr <= ${contSplit.threshold}"
          } else {
            s"$featureStr > ${contSplit.threshold}"
          }
        case catSplit: org.apache.spark.ml.tree.CategoricalSplit =>
          val categoriesStr = catSplit.leftCategories.mkString("{", ",", "}")
          if (left) {
            s"$featureStr in $categoriesStr"
          } else {
            s"$featureStr not in $categoriesStr"
          }
      }
    }
    def subtreeToJSON(node:org.apache.spark.ml.tree.Node,indentFactor: Int = 0): String = {
      val prefix: String = " " * indentFactor
      node match {
        case n:InternalNode=>{
          prefix + "[{\"name\": \"If "+splitToString(n.split, left = true)+"\", \"children\":\n" +
          subtreeToJSON(n.leftChild,indentFactor + 1) +
          prefix + "},{\"name\": \"Else "+splitToString(n.split, left = false)+"\", \"children\":\n" +
          subtreeToJSON(n.rightChild,indentFactor + 1) + "}]"
        }
        case n:LeafNode=>{
          prefix + "[{\"name\":\"Predict: "+n.prediction+" \"}]\n"
        }
      }        
    }
    def treeToJSON(tree: DecisionTreeClassificationModel):String = {
      "{\"name\": \"Root\", \"children\": "+subtreeToJSON(treeModel.rootNode)+"}"
    }
      
    treeToJSON(treeModel)
