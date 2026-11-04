module LordOfTheRings.Objective.Cards.OldLantern (oldLantern) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (lootObjective)

newtype OldLantern = OldLantern ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

oldLantern :: ObjectiveCard OldLantern
oldLantern = objective OldLantern Cards.oldLantern

-- | Loot. If unattached, the first player claims Old Lantern and attaches it to a hero he
-- controls.
instance HasModifiersFor OldLantern where
  getModifiersFor (OldLantern a) = lootObjective a

-- | Action: Exhaust Old Lantern to choose a player to look at the top X cards of his deck,
-- where X is the Discover value of the active location. He adds one of those cards to his
-- hand and places the rest on the bottom of his deck.
instance HasAbilities OldLantern where
  getAbilities (OldLantern a) =
    [ mkAbility a 1 $ ActionAbility (exhaustCost a)
    | AttachedTo _ <- [a.placement]
    , not a.exhausted
    ]

-- | With no active location, or one without the keyword, X is 0.
instance RunMessage OldLantern where
  runMessage msg o@(OldLantern attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      players <- getRemainingPlayers
      active <- select IsActiveLocation
      x <- maybe 0 (fromMaybe 0) <$> traverse getDiscoverValue (headMay active)
      chooseOrRunOneM pid do
        questionLabeled "oldLantern.player"
        causedBy attrs
        targets players \p -> do
          top <- take x . (.deck) <$> getPlayer p
          chooseOneM p do
            questionLabeled "oldLantern.card"
            causedBy attrs
            showing top
            targets top \c -> do
              takeFromDeck p c
              for_ (filter ((/= c.id) . (.id)) top) (moveToBottomOfDeck p)
      pure o
    _ -> OldLantern <$> runMessage msg attrs
