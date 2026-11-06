module LordOfTheRings.Treachery.Cards.GuiltyConscience (guiltyConscience) where

import LordOfTheRings.Card (cdThreatCost, toCardDef)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype GuiltyConscience = GuiltyConscience TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

guiltyConscience :: TreacheryCard GuiltyConscience
guiltyConscience = treachery GuiltyConscience Cards.guiltyConscience

instance HasAbilities GuiltyConscience where
  getAbilities (GuiltyConscience a) = case a.placement of
    AttachedTo host@(HeroTarget _) ->
      [ mkAbility a 1 $ forced RoundEndsWindow
      , mkAbility a 2 $ ActionAbility (ResourceCost host 2)
      ]
    _ -> []

instance RunMessage GuiltyConscience where
  runMessage msg t@(GuiltyConscience attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      heroes <- traverse getHero =<< select AnyHero
      let cost h = fromMaybe 0 $ cdThreatCost (toCardDef h)
          highest = foldr (max . cost) 0 heroes
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "guiltyConscience.attach"
        causedBy attrs
        targets [h | h <- heroes, cost h == highest] \h -> attachTreachery attrs.id h
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      withHost attrs \host -> characterController host >>= traverse_ \pid -> encounterRaisesThreat pid 2
      pure t
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      discard attrs attrs
      pure t
    _ -> GuiltyConscience <$> runMessage msg attrs
