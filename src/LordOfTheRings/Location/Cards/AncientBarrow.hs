module LordOfTheRings.Location.Cards.AncientBarrow (ancientBarrow) where

import LordOfTheRings.Card (toCard)
import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype AncientBarrow = AncientBarrow LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

ancientBarrow :: LocationCard AncientBarrow
ancientBarrow = location AncientBarrow Cards.ancientBarrow

instance HasModifiersFor AncientBarrow where
  getModifiersFor (AncientBarrow a) = modifySelf a [ThreatModifier $ 1 + if a.locationUnderneathFaceup then 0 else length a.underneath]

instance HasAbilities AncientBarrow where
  getAbilities (AncientBarrow a) = [mkAbility a 1 $ forced $ EntersPlayWindow After (toTarget a)]

instance RunMessage AncientBarrow where
  runMessage msg l@(AncientBarrow a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      -- Both sentences belong to the printed Forced ability. Its return clause
      -- is a delayed effect of resolving that ability, not new text to trigger
      -- from the location later (including if its text is subsequently blanked).
      createEffect a a [ReturnUnderneathAlliesWhenExplored] UntilSpent
      push $ DoStep 0 (PlaceUnderneath (toTarget a) [])
      pure l
    DoStep n (PlaceUnderneath (isTarget a -> True) chosen) -> do
      players <- getStagePlayers
      remaining <- getRemainingPlayers
      case drop n (filter (`elem` remaining) players) of
        [] -> placeUnderneath a chosen
        pid : _ -> do
          allies <- filterM eligible =<< select (AllyControlledBy $ PlayerWithId pid)
          if null allies then push $ DoStep (n + 1) (PlaceUnderneath (toTarget a) chosen)
          else chooseOrRunOneM pid do
            questionLabeled "fog.ancientBarrowAlly"
            causedBy a
            targets allies \aid -> do
              card <- toCard <$> getAlly aid
              -- Choose for every player before moving any of the allies: a
              -- leaving-play response must not consume another player's choice.
              push $ DoStep (n + 1) (PlaceUnderneath (toTarget a) (chosen <> [card]))
      pure l
    _ -> AncientBarrow <$> runMessage msg a
   where
    eligible aid = do
      mods <- getModifiers aid
      immune <- immuneToSource (toSource a) aid
      card <- toCard <$> getAlly aid
      cardMods <- getModifiers (CardIdTarget card.id)
      pure $ not immune && CannotBePlacedUnder (toTarget a) `notElem` (mods <> cardMods)
