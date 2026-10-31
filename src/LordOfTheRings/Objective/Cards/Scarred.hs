module LordOfTheRings.Objective.Cards.Scarred (scarred) where

import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype Scarred = Scarred ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Revised Core campaign, attached to a hero. Permanent. Limit 1 per hero.
scarred :: ObjectiveCard Scarred
scarred = objective Scarred Cards.scarred

-- | Forced: After a character you control is destroyed, raise your threat by 1.
instance HasAbilities Scarred where
  getAbilities (Scarred a) = [mkAbility a 1 $ forced $ CharacterDestroyedWindow After | AttachedTo (HeroTarget _) <- [a.placement]]

-- ponytail: "you control" read as the destroyed card's owner, the window carrying no controller
instance RunMessage Scarred where
  runMessage msg s@(Scarred a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [h | AttachedTo (HeroTarget h) <- [a.placement]] \hid -> do
        you <- (.owner) <$> getHero hid
        for_ [() | Window _ (CharacterDestroyed _ (PlayerCard pc)) <- windows, pc.owner == Just you] \_ -> raiseThreat you 1
      pure s
    _ -> Scarred <$> runMessage msg a
