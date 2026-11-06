module LordOfTheRings.Location.Cards.DreadnaughtCoast (mouthsOfAnduin, tolfalasLanding, wickedCove) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype DreadnaughtCoast = DreadnaughtCoast LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

mouthsOfAnduin, tolfalasLanding, wickedCove :: LocationCard DreadnaughtCoast
mouthsOfAnduin = location DreadnaughtCoast Cards.mouthsOfAnduin
tolfalasLanding = location DreadnaughtCoast Cards.tolfalasLanding
wickedCove = location DreadnaughtCoast Cards.wickedCove

instance HasModifiersFor DreadnaughtCoast where
  getModifiersFor (DreadnaughtCoast a) = when (toCardCode a == toCardCode Cards.wickedCove) do
    traverse_ (\lid -> modified a lid [ImmuneToPlayerCardEffects]) =<< select LocationInStagingArea

instance HasAbilities DreadnaughtCoast where
  getAbilities (DreadnaughtCoast a)
    | a.placement /= StagingArea = []
    | toCardCode a == toCardCode Cards.mouthsOfAnduin =
        [mkAbility a 1 $ forced $ WhenRevealedCanceledWindow After]
    | toCardCode a == toCardCode Cards.tolfalasLanding =
        [mkAbility a 1 $ forced $ DrewCardsFromPlayerCardWindow After]
    | otherwise = []

instance RunMessage DreadnaughtCoast where
  runMessage msg l@(DreadnaughtCoast attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      if toCardCode attrs == toCardCode Cards.mouthsOfAnduin
        then revealEncounterCard
        else for_ [(pid, n) | Window _ (CardsDrawn pid n) <- ws] \(pid, n) -> encounterRaisesThreat pid n
      pure l
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      if
        | toCardCode attrs == toCardCode Cards.mouthsOfAnduin -> when (null defenders) revealEncounterCard
        | toCardCode attrs == toCardCode Cards.tolfalasLanding -> do
            player <- getPlayer pid
            encounterRaisesThreat pid (if length player.hand >= 5 then 3 else 1)
        | otherwise -> roundModifiers (CardIdSource cid) eid [ImmuneToPlayerCardEffects, DefenseModifier 2]
      pure l
    _ -> DreadnaughtCoast <$> runMessage msg attrs
