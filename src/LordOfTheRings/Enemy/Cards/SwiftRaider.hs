module LordOfTheRings.Enemy.Cards.SwiftRaider (swiftRaider) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)

newtype SwiftRaider = SwiftRaider EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

swiftRaider :: EnemyCard SwiftRaider
swiftRaider = enemy SwiftRaider Cards.swiftRaider

-- | Boarding 2. Cannot have attachments.
instance HasModifiersFor SwiftRaider where
  getModifiersFor (SwiftRaider a) = modifySelf a [CannotHaveAttachments]

-- | While Swift Raider is in the staging area, it gains: "Forced: After a player commits 4
-- or more characters to a Sailing test or to the quest, Swift Raider engages that player."
instance HasAbilities SwiftRaider where
  getAbilities (SwiftRaider a) =
    [ restricted a i (exists $ EnemyWithId a.id <> EnemyInStagingArea) $ forced w
    | (i, w) <- [(1, CommittedToSailingTestWindow After), (2, AnyCommittedToQuestWindow After)]
    ]

-- | The Dream-chaser counts as 2 characters committed to a Sailing test.
instance RunMessage SwiftRaider where
  runMessage msg e@(SwiftRaider attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      counted <- for [(p, cs) | Window _ (CommittedToSailingTest p cs) <- ws] \(p, cs) -> do
        counts <- for cs \c -> (\ms -> fromMaybe 1 $ headMay [n | CountsAsSailingCharacters n <- ms]) <$> getModifiers c
        pure (p, sum counts)
      engageFirst [p | (p, n) <- counted, n >= 4]
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 ws -> do
      controllers <- for [c | Window _ (CommittedToQuest c) <- ws] characterController
      let counts = Map.fromListWith (+) [(p, 1 :: Int) | Just p <- controllers]
      engageFirst [p | (p, n) <- Map.toList counts, n >= 4]
      pure e
    _ -> SwiftRaider <$> runMessage msg attrs
   where
    engageFirst = traverse_ (`engageEnemy` attrs.id) . take 1
