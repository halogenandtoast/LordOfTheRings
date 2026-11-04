module LordOfTheRings.Treachery.Cards.TheHourOfDoom (theHourOfDoom) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheHourOfDoom = TheHourOfDoom TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theHourOfDoom :: TreacheryCard TheHourOfDoom
theHourOfDoom = treachery TheHourOfDoom Cards.theHourOfDoom

{- | When Revealed: Each player must choose: either raise your threat by 1 for each
character you control, or discard the highest cost ally you control. A player with no
ally raises his threat; with allies tied for the highest cost, he picks one.
Shadow: Discard a non-objective attachment you control.
-}
instance RunMessage TheHourOfDoom where
  runMessage msg t@(TheHourOfDoom attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        characters <- length <$> selectCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
        allies <- traverse getAlly =<< select (AllyControlledBy $ PlayerWithId pid)
        let highest = foldr (max . printedCostOf) 0 allies
            costliest = [toId x | x <- allies, printedCostOf x == highest]
        chooseOrRunOneM pid do
          questionLabeled "theHourOfDoom.choose"
          causedBy attrs
          labeledWith "theHourOfDoom.threat" [intVar "n" characters] $ encounterRaisesThreat pid characters
          unless (null costliest) $ labeled "theHourOfDoom.discard" do
            chooseOrRunOneM pid do
              questionLabeled "theHourOfDoom.discardAlly"
              causedBy attrs
              targets costliest discard
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardNonObjectiveAttachment attrs pid
      pure t
    _ -> TheHourOfDoom <$> runMessage msg attrs
