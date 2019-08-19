package com.kingmanzhang.tmb.curationFX;

import com.kingmanzhang.tmb.curationFX.framework.Injector;
import com.kingmanzhang.tmb.curationFX.gui.TmbCurationFXPresenter;
import com.kingmanzhang.tmb.curationFX.gui.TmbCurationFXView;
import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;
import javafx.scene.shape.Circle;
import javafx.stage.Stage;

public class TmbCurationFX extends Application {

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {

        TmbCurationFXView view = new TmbCurationFXView();
        Scene scene = new Scene(view.getView());
        primaryStage.setTitle("Tumor Mutation Burden CurationFX");
        primaryStage.setScene(scene);

        TmbCurationFXPresenter presenter =(TmbCurationFXPresenter) view.getPresenter();
        primaryStage.show();

    }

    @Override
    public void stop() {
        Injector.forgetAll();
    }


}
