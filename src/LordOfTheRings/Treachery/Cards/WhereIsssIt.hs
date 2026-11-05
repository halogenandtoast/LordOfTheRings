module LordOfTheRings.Treachery.Cards.WhereIsssIt (whereIsssIt) where

import LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim.Helpers (gollumEnemy)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WhereIsssIt = WhereIsssIt TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

whereIsssIt :: TreacheryCard WhereIsssIt
whereIsssIt = treachery WhereIsssIt Cards.whereIsssIt

instance RunMessage WhereIsssIt where
  runMessage msg t@(WhereIsssIt attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      resources <- getBagginsResources
      bilbo <- getBilbo
      gollums <- select gollumEnemy
      chooseOrRunOneM lead do
        questionLabeled "whereIsssIt.choose"
        causedBy attrs
        when (resources >= 2)
          $ labeled "whereIsssIt.discard"
          $ for_ bilbo \b -> removeTokens attrs b Resource 2
        unless (null gollums || isNothing bilbo)
          $ labeled "whereIsssIt.attack"
          $ for_ bilbo \b -> do
            hero <- getHero b
            for_ gollums \g -> do
              attackModifier attrs g (UndefendedDamageTo $ toTarget b)
              immediateAttack hero.owner g
      pure t
    _ -> WhereIsssIt <$> runMessage msg attrs
