module LordOfTheRings.Ally.Cards.DaughterOfTheNimrodel (daughterOfTheNimrodel) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype DaughterOfTheNimrodel = DaughterOfTheNimrodel AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

daughterOfTheNimrodel :: AllyCard DaughterOfTheNimrodel
daughterOfTheNimrodel = ally DaughterOfTheNimrodel Cards.daughterOfTheNimrodel

-- | Action: Exhaust Daughter of the Nimrodel to heal up to 2 damage on any 1 hero.
instance HasAbilities DaughterOfTheNimrodel where
  getAbilities (DaughterOfTheNimrodel a) = [mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a)]

instance RunMessage DaughterOfTheNimrodel where
  runMessage msg s@(DaughterOfTheNimrodel a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- filterM (fmap (> 0) . getDamage) . map toTarget =<< select AnyHero
      unless (null heroes)
        $ push
        $ questionLabelWith (ikey "label.daughterOfTheNimrodel.heal") (Just $ toTarget a) [] pid
          [targetLabel h [HealDamage (toSource a) h 2] | h <- heroes]
      pure s
    _ -> DaughterOfTheNimrodel <$> runMessage msg a
