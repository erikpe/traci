package traci.gui;

import javax.swing.JFrame;

@SuppressWarnings("serial")
public class MainWindow extends JFrame
{
    public MainWindow(final DynamicJPanelDrawArea drawArea, final String title)
    {
        setLocation(200, 200);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        getContentPane().add(drawArea);
        setTitle(title);
        pack();
    }
}
