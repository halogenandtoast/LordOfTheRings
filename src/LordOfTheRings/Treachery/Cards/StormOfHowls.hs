module LordOfTheRings.Treachery.Cards.StormOfHowls (stormOfHowls) where

import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Trait (Trait (Warg))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype StormOfHowls = StormOfHowls TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

stormOfHowls :: TreacheryCard StormOfHowls
stormOfHowls = treachery StormOfHowls Cards.stormOfHowls

{- | Peril. When Revealed: Either the Warg enemy in the staging area with the highest
engagement cost attacks you, or each Warg enemy gets -20 engagement cost until the end of
the round. If no Warg enemy is in the staging area, Storm of Howls gains surge. "You" is
the player who revealed it, who picks among Wargs tied for the highest.
-}
instance RunMessage StormOfHowls where
  runMessage msg t@(StormOfHowls attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      staged <- traverse (\e -> (e,) <$> getEngagementCost e) =<< select (EnemyInStagingArea <> EnemyWithTrait Warg)
      if null staged
        then revealEncounterCard
        else do
          let highest = foldr (max . snd) 0 staged
          wargs <- select (EnemyWithTrait Warg)
          chooseOneM you do
            questionLabeled "stormOfHowls.choose"
            causedBy attrs
            labeled "stormOfHowls.attack" $ chooseOrRunOneM you do
              questionLabeled "stormOfHowls.chooseWarg"
              causedBy attrs
              targets [e | (e, n) <- staged, n == highest] (immediateAttack you)
            labeled "stormOfHowls.engagementCost" $ for_ wargs \e -> roundModifier attrs e (EngagementCostModifier (-20))
      pure t
    _ -> StormOfHowls <$> runMessage msg attrs
